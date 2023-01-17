# Copyright (c) 2023 Yi Yang <kelthuzadx@qq.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>
import copy
import re
import sys

TheBuiltin = [
    {'name': "format", 'param': ['fmt', "args"], 'body': lambda ctxs, args: print(args['fmt'].format(args['args']))},
    {'name': "+", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] + args['b']},
    {'name': "-", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] - args['b']},
    {'name': "*", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] * args['b']},
    {'name': "/", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] / args['b']},
    {'name': "&", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] % args['b']},
]


class Transformer:
    """
    convert source code to s-expressions and make s-expressions to python
    objects to facilitate further use

    :param text: source code
    :return: s-expression list
    """

    def __init__(self, text):
        self.sexprs = []
        self.text = text

    def atom(self, idx):
        def accept_if(i, cond):
            s = self.text[i]
            while i + 1 < len(self.text):
                i += 1
                ch = self.text[i]
                if cond(ch):
                    s += self.text[i]
                else:
                    break
            return s, i

        if self.text[idx].isalpha():
            # identifiers
            return accept_if(idx, lambda c: ('a' <= c <= 'z') or ('A' <= c <= 'Z') or c == '-')
        elif self.text[idx] in '()+*/%><=\'`,':
            # operators
            return self.text[idx], idx + 1
        elif self.text[idx] == '\"':
            # string
            end = self.text.index('\"', idx + 1)
            return self.text[idx:end + 1], end + 1
        elif self.text[idx].isnumeric() or self.text[idx] == '-':
            # minus(-) operator
            if self.text[idx] == '-':
                if idx + 1 < len(self.text) and (not self.text[idx + 1].isnumeric()):
                    return "-", idx + 1
            # 3.14, -3
            return accept_if(idx, lambda c: c.isnumeric() or c == '.')
        elif self.text[idx] == ' ':
            return " ", idx + 1
        return "Invalid-" + self.text[idx], idx + 1

    def check_syntax(self):
        invalid = [item.replace('Invalid-', '') for item in self.sexprs if 'Invalid-' in item]
        if len(invalid) > 0:
            print("SyntaxError: Found invalid content " + str(invalid))
            exit(-1)

    # i.e. ['(', '+', '1', '2', ')'] =-> [['+', '1', '2']]
    def to_py_list(self, i):
        assert self.sexprs[i] == '(', "Must start with ("
        sub = []
        k = i + 1
        while k < len(self.sexprs):
            if self.sexprs[k] == '(':
                self.to_py_list(k)
            if self.sexprs[k] != ')':
                sub.append(self.sexprs[k])
            else:
                break
            k += 1
        assert self.sexprs[k] == ')', "Must start with )"
        del self.sexprs[i:k]
        self.sexprs[i] = sub

    def surround_with_list(self, lst):
        """
        `(...) => [`, (...)] surround with list, note this should be unpacked
         during evaluation time
        """
        k = 0
        while k < len(lst):
            if type(lst[k]) is list:
                self.surround_with_list(lst[k])
            elif lst[k] == "'" or lst[k] == '`' or lst[k] == ',':
                sub = [lst[k], lst[k + 1]]
                del lst[k:k + 1]
                lst[k] = sub
            k += 1

    def make_sexpr_list(self):
        self.text = self.text.replace('\n', '')
        # source code to s-expressions in a whole
        i = 0
        while i < len(self.text):
            lexeme, ni = self.atom(i)
            i = ni
            if lexeme != ' ':
                self.sexprs.append(lexeme)
        self.check_syntax()

        # process top-level s-expressions
        i = 0
        while i < len(self.sexprs):
            if self.sexprs[i] == '(':
                self.to_py_list(i)
            i += 1

        self.surround_with_list(self.sexprs)

        return self.sexprs


class Evaluator:
    """
    Logic of evaluator from practical common lisp book:

    The simplest Lisp forms, atoms, can be divided into two categories: symbols and everything else.
    A symbol, evaluated as a form, is considered the name of a variable and evaluates to the current
    value of the variable.11 I'll discuss in Chapter 6 how variables get their values in the first place.
    You should also note that certain "variables" are that old oxymoron of programming: "constant
    variables." For instance, the symbol PI names a constant variable whose value is the best possible
    floating-point approximation to the mathematical constant pi.

    All other atoms--numbers and strings are the kinds you've seen so far--are self-evaluating objects.
    This means when such an expression is passed to the notional evaluation function, it's simply returned.

    Things get more interesting when we consider how lists are evaluated. All legal list forms start with
    a symbol, but three kinds of list forms are evaluated in three quite different ways. To determine what
    kind of form a given list is, the evaluator must determine whether the symbol that starts the list is
    the name of a function, a macro, or a special operator. If the symbol hasn't been defined yet--as may
    be the case if you're compiling code that contains references to functions that will be defined
    later--it's assumed to be a function name. I'll refer to the three kinds of forms as function call
    forms, macro forms, and special forms.
    """

    def __init__(self):
        self.contexts = [{"func": [func for func in TheBuiltin], "var": []}]
        self.macros = []

    def find_func(self, name):
        for i in reversed(range(len(self.contexts))):
            for func in self.contexts[i]['func']:
                if func['name'] == name:
                    return func
        return None

    def find_var(self, name):
        for var in self.contexts[-1]['var']:
            if var['name'] == name:
                return var['value']

    def add_variable(self, var, ctx=None):
        if ctx is None:
            self.contexts[-1]['var'].append(var)
        else:
            ctx['var'].append(var)

    def add_func(self, func, ctx=None):
        if ctx is None:
            self.contexts[-1]['func'].append(func)
        else:
            ctx['func'].append(func)

    def add_macro(self, macro):
        self.macros.append(macro)

    def new_context(self):
        return {"func": [], "var": []}

    def enter_context(self, ctx=None):
        if ctx is None:
            self.contexts.append({"func": [], "var": []})
        else:
            self.contexts.append(ctx)

    def leave_context(self):
        del self.contexts[-1]

    def allow_evaluation(self, lst):
        """
        [, (...)] => (...)
        """
        i = 0
        while i < len(lst):
            if self.is_sexpr_list(lst[i]):
                self.allow_evaluation(lst[i])
            elif lst[i] == ',':
                next_value = self.eval_list(lst[i + 1])
                del lst[i:i + 1]
                lst[i] = next_value
            i += 1

    def eval_block(self, lst):
        ret_val = None
        for i in range(len(lst)):
            var = self.eval_list(lst[i])
            if i == len(lst) - 1:
                ret_val = var
        return ret_val

    def expand_macro_call(self, macro, sexpr_list):
        assert len(macro['param']) == len(
            sexpr_list) - 1, f"Macro {macro['name']} expects {len(macro['param'])} parameter but found {len(sexpr_list) - 1}"
        # Prologue, prepare arguments
        new_ctx = self.new_context()
        for i in range(0, len(macro['param'])):
            # pass source code as arguments directly
            self.add_variable({"name": macro['param'][i], "value": sexpr_list[i + 1]}, new_ctx)

        # Call builtin or user defined function
        self.enter_context(new_ctx)
        # Clone macro code body as template for later code generation
        body = copy.deepcopy(macro['body'])
        for i, expr in enumerate(body):
            val = self.eval_list(expr)
            # Replace macro body with evaluated value
            body[i] = val
        self.leave_context()
        # TODO: revise this later
        return body[0]

    def call(self, func, lst):
        assert len(func['param']) == len(
            lst) - 1, f"Function {func['name']} expects {len(func['param'])} parameter but found {len(lst) - 1}"
        # Prologue, prepare arguments
        new_ctx = self.new_context()
        for i in range(0, len(func['param'])):
            self.add_variable({"name": func['param'][i], "value": self.eval_list(lst[i + 1])}, new_ctx)

        # Call builtin or user defined function
        self.enter_context(new_ctx)
        body = func['body']
        if callable(body):
            args = {}
            for item in new_ctx['var']:
                args[item['name']] = item['value']
            ret_val = body(self.contexts, args)
        else:
            ret_val = self.eval_block(body)
        self.leave_context()

        Evaluator.log_trace(lst, ret_val)
        return ret_val

    def eval_atom(self, atom):
        # If it's a self-evaluating atom
        if re.match("\".*\"", atom) or re.match("-?[0-9][0-9.]*", atom):
            return eval(atom)
        elif atom == "true" or atom == "false":
            return eval(atom.title())
        else:
            # Otherwise, it's a representation of variable
            val = self.find_var(atom)
            if val is not None:
                return val

        raise ValueError(f'Unknown single s-expression {atom}')

    def eval_list(self, lst):
        if not Evaluator.is_sexpr_list(lst):
            val = self.eval_atom(lst)
            Evaluator.log_trace(lst, val)
            return val

        if len(lst) == 0:
            Evaluator.log_trace(lst, None)
            return None

        action = lst[0]
        if self.is_sexpr_list(action):
            # (<func> ...)
            action = self.eval_list(action)
            lst[0] = action

        if "body" in action:
            # (<func> ...)
            func = action
            val = self.call(func, lst)
            Evaluator.log_trace(lst, func)
            return val
        elif action == 'quote' or action == "'":
            # (quote a)
            # '(a b)
            # Note, quote has been processed when making the s-expression list
            # so any form of ' ... should be (` ...), i.e. its operand is surrounded
            # with list, we need to unpack it here
            Evaluator.log_trace(lst, lst[1])
            return lst[1]
        elif action == '`':
            # `((+ 1 2) ,(+1 2))
            ret_val = lst[1]
            self.allow_evaluation(ret_val)
            Evaluator.log_trace(lst, ret_val)
            return ret_val
        elif action == 'block':
            # (block (..) (..) (..))
            exprs = lst[1:]
            ret_val = self.eval_block(exprs)
            Evaluator.log_trace(lst, ret_val)
            return ret_val
        elif action == 'if':
            # (if cond (then-block) (else-block）)
            # (if cond (then-block))
            cond = self.eval_list(lst[1])
            val = None
            if cond:
                val = self.eval_list(lst[2])
            else:
                if len(lst) == 4:
                    val = self.eval_list(lst[3])
            Evaluator.log_trace(lst, val)
            return val
        elif action == 'let':
            # (let ((a 11) (b 12) c) ...)
            var_bindings = lst[1]
            new_ctx = self.new_context()
            for var_binding in var_bindings:
                if Evaluator.is_sexpr_list(var_binding):
                    assert len(var_binding) == 2, "must be initialization of var binding"
                    name = var_binding[0]
                    value = self.eval_list(var_binding[1])
                    self.add_variable({"name": name, "value": value}, new_ctx)
                else:
                    assert len(var_binding) == 1, "must be default initialization of var binding"
                    name = var_binding[0]
                    self.add_variable({"name": name, "value": None}, new_ctx)

            self.enter_context(new_ctx)
            ret_val = self.eval_block(lst[2:])
            self.leave_context()
            Evaluator.log_trace(lst, ret_val)
            return ret_val
        elif action == 'def':
            # (def name () ...)
            func = {'name': lst[1], 'param': lst[2], 'body': lst[3:]}
            self.add_func(func)
            Evaluator.log_trace(lst, func)
            return func
        elif action == 'lambda':
            # (lambda () ...)
            func = {'name': '<lambda>', 'param': lst[1], 'body': lst[2:]}
            self.add_func(func)
            Evaluator.log_trace(lst, func)
            return func
        elif action == 'macro':
            raise ValueError("Macro should be already collected and expanded")
        else:
            # (foo ...)
            var = self.find_var(action)
            if var is not None:
                Evaluator.log_trace(lst, var)
                return var

            func = self.find_func(action)
            if func is not None:
                val = self.call(func, lst)
                Evaluator.log_trace(lst, val)
                return val
            macro = self.find_macro(action)
            if macro is not None:
                raise ValueError(f"Macro {action} is not expanded!")
        raise ValueError(f'Unknown s-expression {lst}')

    def collect_macro(self):
        for idx, expr in enumerate(sexpr_list):
            if self.is_sexpr_list(expr) and len(expr) > 0:
                if expr[0] == 'macro':
                    macro = {'name': expr[1], 'param': expr[2], 'body': expr[3:]}
                    self.add_macro(macro)
                    del sexpr_list[idx]

    def find_macro(self, name):
        for macro in self.macros:
            if macro['name'] == name:
                return macro
        return None

    def expand_macro(self, sexpr_list):
        if not self.is_sexpr_list(sexpr_list):
            return None
        for idx, sexpr in enumerate(sexpr_list):
            if self.is_sexpr_list(sexpr) and len(sexpr) > 0:
                if type(sexpr[0]) is str:
                    macro = self.find_macro(sexpr[0])
                    if macro is not None:
                        assert len(sexpr[1:]) == len(macro['param']), "num of macro parameters mismatched"
                        expanded = self.expand_macro_call(macro, sexpr)
                        sexpr_list[idx] = expanded
                        continue
                self.expand_macro(sexpr)

    @staticmethod
    def is_sexpr_list(a):
        return type(a) is list

    @staticmethod
    def log_trace(a, b):
        print(f"== Evaluate S-expression {a} that produces {b}({type(b)})")


def generate_sexprs_from_file(path):
    with open(path) as file:
        content = file.read()
        t = Transformer(content)
        sexpr_list = t.make_sexpr_list()
        return sexpr_list


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("No source file")
        exit(-1)

    sexpr_list = generate_sexprs_from_file(sys.argv[1])
    e = Evaluator()
    print(f"== Initial S-expression {sexpr_list}")
    e.collect_macro()
    e.expand_macro(sexpr_list)
    print(f"== Macro Expanded {sexpr_list}")
    e.eval_block(sexpr_list)
