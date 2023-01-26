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
import os
import re
import sys

# Steed - an easy-to-use dialect of lisp
#
# A list can be a lot of things in Lisp. In the most general sense, a list
# can be either a program or data. And because lists can themselves be made
# of other lists, you can have arbitrary combinations of data and programs
# mixed at different levels of list structure.
#
# Atoms are separated by whitespace or parentheses Now that you can recognize
# a list, you'd like to have a name for the things that appear between the
# parentheses the things that are not themselves lists, but rather words
# and numbers. These things are called atoms.
enable_debug = False


class SyntaxParser:

    def __init__(self, text):
        self.forms = []
        self.text = text

    @staticmethod
    def parse_file(file_path):
        with open(file_path) as file:
            content = file.read()
            t = SyntaxParser(content)
            sexpr_list = t.make_form_list()
            return sexpr_list

    @staticmethod
    def parse_text(content):
        t = SyntaxParser(content)
        sexpr_list = t.make_form_list()
        return sexpr_list

    def syntax_check(self):
        for item in self.forms:
            if type(item) is str and 'Invalid-' in item:
                raise RuntimeError(f"Syntax error at {item}")

    def to_py_list(self, i):
        # i.e. ['(', '+', '1', '2', ')'] =-> [['+', '1', '2']]
        assert self.forms[i] == '(', "Must start with ("
        sub = []
        k = i + 1
        while k < len(self.forms):
            if self.forms[k] == '(':
                self.to_py_list(k)
            if self.forms[k] != ')':
                sub.append(self.forms[k])
            else:
                break
            k += 1
        assert self.forms[k] == ')', "Must start with )"
        del self.forms[i:k]
        self.forms[i] = sub

    def surround_with_list(self, lst):
        # `(...) => [`, (...)] surround with list, note this should be unpacked
        # during evaluation time
        k = 0
        while k < len(lst):
            if type(lst[k]) is list:
                self.surround_with_list(lst[k])
            elif lst[k] == "'" or lst[k] == '`' or lst[k] == ',':
                sub = [lst[k], lst[k + 1]]
                del lst[k:k + 1]
                lst[k] = sub
            k += 1

    def remove_comments(self):
        lines = self.text.split("\n")
        lines = [s for s in lines if not s.strip().startswith(";")]
        lines = [re.sub(r';.*', '', s) for s in lines]
        self.text = ' '.join(lines)
        self.text = self.text.replace('\n', ' ')

    def make_atom(self, idx):
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
            s, i = accept_if(idx, lambda c: ('a' <= c <= 'z') or ('A' <= c <= 'Z') or c == '-')
            if s == 'true' or s == 'false':
                return eval(s.title()), i
            elif s == 'nil':
                return None, i
            return s, i
        elif self.text[idx] in '()+*/%\'`,':
            # operators
            return self.text[idx], idx + 1
        elif self.text[idx] in '><=!':
            tk = self.text[idx]
            if self.text[idx + 1] == '=':
                tk += '='
                return tk, idx + 2
            return tk, idx + 1
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
            s, i = accept_if(idx, lambda c: c.isnumeric() or c == '.')
            return eval(s), i
        elif self.text[idx] == ' ':
            return " ", idx + 1
        return "Invalid-" + self.text[idx], idx + 1

    def make_form_list(self):
        # remove comment lines
        self.remove_comments()

        # source code to s-expressions in a whole
        i = 0
        while i < len(self.text):
            lexeme, ni = self.make_atom(i)
            i = ni
            if lexeme != ' ':
                self.forms.append(lexeme)
        self.syntax_check()

        # process top-level s-expressions
        i = 0
        while i < len(self.forms):
            if self.forms[i] == '(':
                self.to_py_list(i)
            i += 1

        self.surround_with_list(self.forms)

        return self.forms


def steed_asert(args):
    if not args['cond']:
        raise RuntimeError(args['msg'])


TheBuiltin = [
    {'name': "format", 'param': ['fmt', "args"], 'body': lambda args: print(args['fmt'].format(args['args']))},
    {'name': "assert", 'param': ['cond', "msg"], 'body': lambda args: steed_asert(args)},
    {'name': "+", 'param': ['a', "b"], 'body': lambda args: args['a'] + args['b']},
    {'name': "-", 'param': ['a', "b"], 'body': lambda args: args['a'] - args['b']},
    {'name': "*", 'param': ['a', "b"], 'body': lambda args: args['a'] * args['b']},
    {'name': "/", 'param': ['a', "b"], 'body': lambda args: args['a'] / args['b']},
    {'name': "&", 'param': ['a', "b"], 'body': lambda args: args['a'] % args['b']},
    {'name': ">", 'param': ['a', "b"], 'body': lambda args: args['a'] > args['b']},
    {'name': ">=", 'param': ['a', "b"], 'body': lambda args: args['a'] >= args['b']},
    {'name': "<", 'param': ['a', "b"], 'body': lambda args: args['a'] < args['b']},
    {'name': "<=", 'param': ['a', "b"], 'body': lambda args: args['a'] <= args['b']},
    {'name': "!=", 'param': ['a', "b"], 'body': lambda args: args['a'] != args['b']},
    {'name': "==", 'param': ['a', "b"], 'body': lambda args: args['a'] == args['b']},
    {'name': "and", 'param': ['a', "b"], 'body': lambda args: args['a'] and args['b']},
    {'name': "or", 'param': ['a', "b"], 'body': lambda args: args['a'] or args['b']},
    {'name': "not", 'param': ['a'], 'body': lambda args: not args['a']},
    {'name': "eq", 'param': ['a', 'b'], 'body': lambda args: args['a'] == args['b']},
]


# A form can be either an atom or a list. The important thing is that the form is
# meant to be evaluated.
#
# Evaluation is simple if the form is an atom. Lisp treats the atom as a name, and
# retrieves the value for the name (if a value exists)
# If a form is a list, then the first element must be either a symbol or a special
# form called a lambda expression. The symbol must name a function. In Lisp, the
# symbols +, -, *, and / name the four common arithmetic operations: addition,
# subtraction, multiplication, and division. Each of these symbols has an associated
# function that performs the arithmetic operation.
class Evaluator:
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
                return var
        return None

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

    def allow_eval(self, lst):
        # [, (...)] => (...)
        i = 0
        while i < len(lst):
            if self.is_type_list(lst[i]):
                self.allow_eval(lst[i])
            elif lst[i] == ',':
                next_value = self.eval_form(lst[i + 1])
                del lst[i:i + 1]
                lst[i] = next_value
            i += 1

    def trace_eval(self, a, b):
        if enable_debug:
            print(f"== Eval {'list' if self.is_type_list(a) else 'atom'} {a} => {b}")

    def macro_call(self, macro, sexpr_list):
        # Macros returns a form, not a value
        assert len(macro['param']) == len(sexpr_list) - 1, "parameter mismatched"
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
            val = self.eval_form(expr)
            # Replace macro body with evaluated value
            body[i] = val
        self.leave_context()
        # TODO: revise this later
        return body[0]

    def call(self, func, lst):
        # Functions return a value as expected
        assert len(func['param']) == len(lst) - 1, "parameter mismatched"
        # Prologue, prepare arguments
        new_ctx = self.new_context()
        for i in range(0, len(func['param'])):
            self.add_variable({"name": func['param'][i], "value": self.eval_form(lst[i + 1])}, new_ctx)

        # Call builtin or user defined function
        self.enter_context(new_ctx)
        body = func['body']
        if callable(body):
            args = {}
            for item in new_ctx['var']:
                args[item['name']] = item['value']
            ret_val = body(args)
        else:
            ret_val = self.eval_block(body)
        self.leave_context()
        return ret_val

    def eval_top_level(self, lst):
        if enable_debug:
            print(f"Initial S-expression{lst}")
        self.collect_macro(lst)
        self.expand_macro(lst)
        if enable_debug:
            print(f"Macro Expanded{lst}")
        self.eval_block(lst)

    def eval_block(self, lst):
        ret_val = None
        for i in range(len(lst)):
            var = self.eval_form(lst[i])
            if i == len(lst) - 1:
                ret_val = var
        return ret_val

    def eval_atom(self, atom):
        # Self-evaluating atom, e.g. true, 3.14, -5, nil
        if atom is str or type(atom) is int or type(atom) is float or \
                type(atom) is bool or atom is None:
            return atom
        # Special case, we need to distinguish "foo"(string) and foo(symbol)
        # even if "foo" is self-evaluating atom
        elif re.match('".*?"', atom):
            return eval(atom)
        # identifier
        else:
            # Otherwise, it's a representation of variable
            var = self.find_var(atom)
            if var['value'] is not None:
                return var['value']

        raise RuntimeError(f'Unknown form {atom} during syntax parsing')

    def eval_form(self, lst):
        if not self.is_type_list(lst):
            val = self.eval_atom(lst)
            self.trace_eval(lst, val)
            return val

        # () is evaluated to nil, which is equivalent to "None"
        # in our implementation
        if len(lst) == 0:
            self.trace_eval(lst, None)
            return None

        action = lst[0]
        if self.is_type_list(action):
            # (<func> ...)
            action = self.eval_form(action)
            lst[0] = action

        if "body" in action:
            # (<func> ...)
            func = action
            val = self.call(func, lst)
            self.trace_eval(lst, func)
            return val
        elif action == 'quote' or action == "'":
            # (quote a)
            # '(a b)
            # Note, quote has been processed when making the s-expression list
            # so any form of ' ... should be (` ...), i.e. its operand is surrounded
            # with list, we need to unpack it here
            self.trace_eval(lst, lst[1])
            return lst[1]
        elif action == '`':
            # `((+ 1 2) ,(+1 2))
            ret_val = lst[1]
            self.allow_eval(ret_val)
            self.trace_eval(lst, ret_val)
            return ret_val
        elif action == 'block':
            # (block (..) (..) (..))
            exprs = lst[1:]
            ret_val = self.eval_block(exprs)
            self.trace_eval(lst, ret_val)
            return ret_val
        elif action == 'if':
            # (if cond (then-block) (else-block)? )
            cond = self.eval_form(lst[1])
            val = None
            if cond:
                val = self.eval_form(lst[2])
            else:
                if len(lst) == 4:
                    val = self.eval_form(lst[3])
            self.trace_eval(lst, val)
            return val
        elif action == 'for':
            # (for (i 0) (< i 10) (+ i 1) (...))
            init = lst[1]
            cond = lst[2]
            stride = lst[3]
            body = lst[4]
            new_ctx = self.new_context()
            self.add_variable({'name': init[0], 'value': self.eval_form(init[1])}, new_ctx)

            self.enter_context(new_ctx)
            while self.eval_form(cond):
                self.eval_form(body)
                new_val = self.eval_form(stride)
                self.find_var(init[0])['value'] = new_val
            self.leave_context()
            self.trace_eval(lst, None)
            return None
        elif action == 'let':
            # (let ((a 11) (b 12) c) ...)
            var_bindings = lst[1]
            new_ctx = self.new_context()
            for var_binding in var_bindings:
                if self.is_type_list(var_binding):
                    assert len(var_binding) == 2, "must be initialization of var binding"
                    name = var_binding[0]
                    value = self.eval_form(var_binding[1])
                    self.add_variable({"name": name, "value": value}, new_ctx)
                else:
                    assert len(var_binding) == 1, "must be default initialization of var binding"
                    name = var_binding[0]
                    self.add_variable({"name": name, "value": None}, new_ctx)

            self.enter_context(new_ctx)
            ret_val = self.eval_block(lst[2:])
            self.leave_context()
            self.trace_eval(lst, ret_val)
            return ret_val
        elif action == 'setq':
            # (setq name "Zhao")
            exprs = lst[1:]
            if len(exprs) % 2 != 0:
                raise RuntimeError("expect even number of arguments")
            for i in range(0, len(exprs), 2):
                symbol = exprs[i]
                value = self.eval_form(exprs[i + 1])
                self.add_variable({'name': symbol, 'value': value})
            self.trace_eval(lst, None)
            return None
        elif action == 'cons':
            # (cons 1 (cons 2 (cons 3 nil)))
            first = self.eval_form(lst[1])
            second = self.eval_form(lst[2])
            value = [first]
            if second is not None:
                value += second
            self.trace_eval(lst, value)
            return value
        elif action == 'first':
            # (first '(1 2 4 5 6))
            value = self.eval_form(lst[1])
            if not self.is_type_list(value):
                raise RuntimeError("Expect an list as its argument")
            value = value[0]
            self.trace_eval(lst, value)
            return value
        elif action == 'rest':
            # (rest '(1 2 4 5 6))
            value = self.eval_form(lst[1])
            if not self.is_type_list(value):
                raise RuntimeError("Expect an list as its argument")
            value = value[1:]
            self.trace_eval(lst, value)
            return value

        elif action == 'list':
            # (list 1 2 3)
            value = []
            for expr in lst[1:]:
                value += [self.eval_form(expr)]
            self.trace_eval(lst, value)
            return value
        elif action == 'defun':
            # (def name () ...)
            func = {'name': lst[1], 'param': lst[2], 'body': lst[3:]}
            self.add_func(func)
            self.trace_eval(lst, func)
            return func
        elif action == 'lambda':
            # (lambda () ...)
            func = {'name': '<lambda>', 'param': lst[1], 'body': lst[2:]}
            self.add_func(func)
            self.trace_eval(lst, func)
            return func
        elif action == 'defmacro':
            raise RuntimeError("Macro should be already collected and expanded")
        else:
            # (foo ...)
            var = self.find_var(action)
            if var is not None:
                self.trace_eval(lst, var['value'])
                return var['value']

            func = self.find_func(action)
            if func is not None:
                val = self.call(func, lst)
                self.trace_eval(lst, val)
                return val

            macro = self.find_macro(action)
            if macro is not None:
                raise RuntimeError(f"Macro {action} is not expanded!")

        raise RuntimeError(f'Unknown form {lst} during evaluation')

    def collect_macro(self, sexpr_list):
        # (macro name ()...)
        idx = 0
        while idx < len(sexpr_list):
            expr = sexpr_list[idx]
            if self.is_type_list(expr) and len(expr) > 0:
                if expr[0] == 'defmacro':
                    macro = {'name': expr[1], 'param': expr[2], 'body': expr[3:]}
                    self.add_macro(macro)
                    if enable_debug:
                        print(f"Find macro definition {macro['name']}")
                    del sexpr_list[idx]
                    continue
            idx += 1

    def find_macro(self, name):
        for macro in self.macros:
            if macro['name'] == name:
                return macro
        return None

    def expand_macro(self, sexpr_list):
        if not self.is_type_list(sexpr_list):
            return None
        for idx, sexpr in enumerate(sexpr_list):
            if self.is_type_list(sexpr) and len(sexpr) > 0:
                if type(sexpr[0]) is str:
                    macro = self.find_macro(sexpr[0])
                    if macro is not None:
                        assert len(sexpr[1:]) == len(macro['param']), "num of macro parameters mismatched"
                        expanded = self.macro_call(macro, sexpr)
                        sexpr_list[idx] = expanded
                        continue
                self.expand_macro(sexpr)

    @staticmethod
    def is_type_list(a):
        return type(a) is list


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("No source file")
        exit(-1)

    script_name = ""
    for arg in sys.argv:
        if arg == '--debug':
            enable_debug = True
        elif arg.endswith('.st'):
            script_name = arg

    src_dir = os.path.dirname(os.path.realpath(__file__))
    lst_stdlib = SyntaxParser.parse_file(src_dir + "/stdlib.st")
    lst_source = SyntaxParser.parse_file(script_name)
    e = Evaluator()
    e.eval_top_level(lst_stdlib)
    e.eval_top_level(lst_source)
