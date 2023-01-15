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

    def unit(self, idx):
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
        elif self.text[idx] in '()+*/%\'':
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
        # delete elements i range of [), so we can replace these elements with sub
        del self.sexprs[i:k]
        self.sexprs[i] = sub

    def make_sexpr_list(self):
        self.text = self.text.replace('\n', '')
        # source code to s-expressions in a whole
        i = 0
        while i < len(self.text):
            lexeme, ni = self.unit(i)
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
        return self.sexprs


class Evaluator:
    def __init__(self, sexprs):
        self.sexprs = sexprs
        self.contexts = [{"func": [func for func in TheBuiltin], "var": []}]

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

    def new_context(self):
        return {"func": [], "var": []}

    def enter_context(self, ctx=None):
        if ctx is None:
            self.contexts.append({"func": [], "var": []})
        else:
            self.contexts.append(ctx)

    def leave_context(self):
        del self.contexts[-1]

    def eval_block(self, body_list):
        ret_val = None
        for i in range(len(body_list)):
            var = self.eval_sexpr_list(body_list[i])
            Evaluator.log_trace(body_list, var)
            if i == len(body_list) - 1:
                ret_val = var
        return ret_val

    def call(self, func, sexprs):
        assert len(func['param']) == len(
            sexprs) - 1, f"Function {func['name']} expects {len(func['param'])} parameter but found {len(sexprs) - 1}"
        # Prologue, prepare arguments
        new_ctx = self.new_context()
        for i in range(0, len(func['param'])):
            self.add_variable({"name": func['param'][i], "value": self.eval_sexpr_list(sexprs[i + 1])}, new_ctx)

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

        Evaluator.log_trace(sexprs, ret_val)
        return ret_val

    def eval_top_level(self):
        for i in sexpr_list:
            val = self.eval_sexpr_list(i)
            print(val)

    def eval_sexpr(self, sexpr):
        if re.match("\".*\"", sexpr) or re.match("-?[0-9][0-9.]*", sexpr):
            return eval(sexpr)
        else:
            val = self.find_var(sexpr)
            if val is not None:
                return val

        raise ValueError(f"Unknown single s-expression {sexpr}")

    def eval_sexpr_list(self, sexpr_list):
        if not Evaluator.is_sexpr_list(sexpr_list):
            val = self.eval_sexpr(sexpr_list)
            Evaluator.log_trace(sexpr_list, val)
            return val

        if len(sexpr_list) == 0:
            Evaluator.log_trace(sexpr_list, None)
            return None

        action = sexpr_list[0]
        if type(action) is list:
            # (<func> ...)
            action = self.eval_sexpr_list(action)
            sexpr_list[0] = action

        if "body" in action:
            # (<func> ...)
            func = action
            val = self.call(func, sexpr_list)
            Evaluator.log_trace(sexpr_list, func)
            return val
        elif action == 'quote' or action == '\'':
            # (quote ...)
            Evaluator.log_trace(sexpr_list, sexpr_list)
            return sexpr_list
        elif action == 'let':
            # (let ((a 11) (b 12) c) ...)
            var_bindings = sexpr_list[1]
            new_ctx = self.new_context()
            for var_binding in var_bindings:
                if Evaluator.is_sexpr_list(var_binding):
                    assert len(var_binding) == 2, "must be initialization of var binding"
                    name = var_binding[0]
                    value = self.eval_sexpr_list(var_binding[1])
                    self.add_variable({"name": name, "value": value}, new_ctx)
                else:
                    assert len(var_binding) == 1, "must be default initialization of var binding"
                    name = var_binding[0]
                    self.add_variable({"name": name, "value": None}, new_ctx)

            self.enter_context(new_ctx)
            ret_val = self.eval_block(sexpr_list[2:])
            self.leave_context()
            Evaluator.log_trace(sexpr_list, ret_val)
            return ret_val
        elif action == 'def':
            # (def name () ...)
            func = {'name': sexpr_list[1], 'param': sexpr_list[2], 'body': sexpr_list[3:]}
            self.add_func(func)
            Evaluator.log_trace(sexpr_list, func)
            return func
        elif action == 'lambda':
            # (lambda () ...)
            func = {'name': '<lambda>', 'param': sexpr_list[1], 'body': sexpr_list[2:]}
            self.add_func(func)
            Evaluator.log_trace(sexpr_list, func)
            return func
        else:
            # (foo ...)
            var = self.find_var(action)
            if var is not None:
                Evaluator.log_trace(sexpr_list, var)
                return var

            func = self.find_func(action)
            if func is not None:
                val = self.call(func, sexpr_list)
                Evaluator.log_trace(sexpr_list, val)
                return val

        raise ValueError("Should not reach here")

    @staticmethod
    def is_sexpr_list(a):
        return type(a) is list

    @staticmethod
    def log_trace(a, b):
        print(f"== Evaluate S-expression {a} that produces {b}({type(b)})")


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("No source file")
        exit(-1)

    with open(sys.argv[1]) as file:
        content = file.read()
        t = Transformer(content)
        sexpr_list = t.make_sexpr_list()

    e = Evaluator(sexpr_list)
    e.eval_top_level()
