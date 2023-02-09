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
from src.context import Context, CtlFlag
from src.parser import SyntaxParser
from src.utils import is_type_list

ENABLE_DEBUG = False


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


def is_logically_true(a):
    # all objects are true except for "nil"
    return a is not None


def classify_lambda_list_keywords(params):
    category = []
    i = 0
    while i < len(params):
        keyword_name = params[i] if params[i].startswith('&') else "&normal"
        keyword_params = []
        k = i + 1 if params[i].startswith('&') else i
        while k < len(params):
            if params[k].startswith('&'):
                break
            keyword_params.append(params[k])
            k += 1
        category.append({"keyword_name": keyword_name, "keyword_params": keyword_params})
        i = k
    return category


class Evaluator:
    def __init__(self):
        self.context = Context()

    @staticmethod
    def trace_eval(a, b):
        if ENABLE_DEBUG:
            print(f"== Eval {'list' if is_type_list(a) else 'atom'} {a} => {b}")

    def eval_quote(self, lst):
        # ,(+ 1 2) => 3
        # ,@(1 2)  => 1 2
        i = 0
        while i < len(lst):
            if is_type_list(lst[i]):
                if len(lst[i]) > 0 and (lst[i][0] == ',' or lst[i][0] == ',@'):
                    assert len(lst[i]) == 2, "sanity check"
                    next_value = self.eval_form(lst[i][1])
                    if lst[i][0] == ',':
                        lst[i] = next_value
                    else:
                        if not is_type_list(next_value):
                            raise RuntimeError(f"expect an list but got {next_value}")
                        lst[i:i + 1] = next_value
                        pass
                else:
                    self.eval_quote(lst[i])
            i += 1

    def eval_lambda_list(self, params, lst, ctx, is_macro_call):
        lambda_list_keywords = classify_lambda_list_keywords(params)
        param_i = 0
        for keyword in lambda_list_keywords:
            kw_name = keyword['keyword_name']
            kw_params = keyword['keyword_params']

            if kw_name == '&normal':
                # (defun foo (a a b c) ...)
                # (foo 1 2 3)
                while param_i < len(kw_params):
                    if not is_macro_call:
                        arg_val = self.eval_form(lst[param_i + 1]) if param_i + 1 < len(lst) else None
                    else:
                        arg_val = lst[param_i + 1]
                    self.context.add_var(kw_params[param_i], arg_val, ctx)
                    param_i += 1
            elif kw_name == '&key':
                # (defun foo (&key a b c) ...)
                # (foo :a 2 :b)
                while param_i < len(kw_params):
                    kw_param_idx = -1
                    for t in range(1, len(lst)):
                        if lst[t] == f":{kw_params[param_i]}":
                            kw_param_idx = t
                            break

                    if kw_param_idx != -1:
                        if not is_macro_call:
                            # eval argument as usual
                            arg_val = self.eval_form(lst[kw_param_idx + 1])
                        else:
                            # pass source code as arguments directly
                            arg_val = lst[kw_param_idx + 1]
                    else:
                        arg_val = None
                    self.context.add_var(kw_params[param_i], arg_val, ctx)
                    param_i += 1
            elif kw_name == '&body' or kw_name == '&rest':
                if len(kw_params) != 1:
                    raise RuntimeError("&body parameter is invalid")
                arg_val = []
                for t in range(1 + param_i, len(lst)):
                    arg_val.append(self.eval_form(lst[t]))
                self.context.add_var(kw_params[0], arg_val, ctx)
            else:
                pass

    def eval_call(self, func, lst, is_macro_call=False):
        # Prologue, process lambda list
        new_ctx = self.context.new_context()
        self.eval_lambda_list(func['param'], lst, new_ctx, is_macro_call)

        # {
        self.context.enter_context(new_ctx)
        if not is_macro_call:
            # Call builtin or user defined function
            body = func['body']
            if callable(body):
                args = {}
                for item in new_ctx['var']:
                    args[item['name']] = item['value']
                ret_val = body(args)
            else:
                ret_val = self.eval_block(body)
        else:
            # Clone macro code body as template for later code generation
            body = copy.deepcopy(func['body'])
            for i, expr in enumerate(body):
                val = self.eval_form(expr)
                # Replace macro body with evaluated value
                body[i] = val
            # TODO: revise below line later
            ret_val = body[0]

        self.context.leave_context()
        # }
        return ret_val

    def eval_top_level(self, lst):
        if ENABLE_DEBUG:
            print(f"Initial S-expression{lst}")
        self.collect_macro(lst)
        self.expand_macro(lst)
        if ENABLE_DEBUG:
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
            var = self.context.find_var(atom)
            if var is not None:
                return var['value']

        raise RuntimeError(f'Unknown form {atom} during syntax parsing')

    def eval_form(self, lst):
        if not is_type_list(lst):
            val = self.eval_atom(lst)
            Evaluator.trace_eval(lst, val)
            return val

        # () is evaluated to nil, which is equivalent to "None"
        # in our implementation
        if len(lst) == 0:
            Evaluator.trace_eval(lst, None)
            return None

        # eval the following special forms
        action = lst[0]
        if is_type_list(action):
            # (<func> ...)
            action = self.eval_form(action)
            lst[0] = action

        if type(action) is dict and "body" in action:
            # (<func> ...)
            func = action
            val = self.eval_call(func, lst)
            Evaluator.trace_eval(lst, func)
            return val
        elif action == 'quote' or action == "'":
            # (quote a)
            # '(a b)
            # Note, quote has been processed when making the s-expression list
            # so any form of ' ... should be (` ...), i.e. its operand is surrounded
            # with list, we need to unpack it here
            Evaluator.trace_eval(lst, lst[1])
            return lst[1]
        elif action == '`':
            # `((+ 1 2) ,(+1 2) ,@(1 2))
            ret_val = lst[1]
            self.eval_quote(ret_val)
            Evaluator.trace_eval(lst, ret_val)
            return ret_val
        elif action == 'progn':
            # (progn (..) (..) (..))
            exprs = lst[1:]
            ret_val = self.eval_block(exprs)
            Evaluator.trace_eval(lst, ret_val)
            return ret_val
        elif action == 'if':
            # (if cond (then-block) (else-block)? )
            cond = self.eval_form(lst[1])
            val = None
            if is_logically_true(cond):
                val = self.eval_form(lst[2])
            else:
                if len(lst) == 4:
                    val = self.eval_form(lst[3])
            Evaluator.trace_eval(lst, val)
            return val
        elif action == 'for':
            # (for (i 0) (< i 10) (+ i 1) (...))
            init = lst[1]
            cond = lst[2]
            stride = lst[3]
            body = lst[4]
            new_ctx = self.context.new_context()
            self.context.add_var(init[0], self.eval_form(init[1]), new_ctx)

            self.context.enter_context(new_ctx)
            while is_logically_true(self.eval_form(cond)):
                self.eval_form(body)
                new_val = self.eval_form(stride)
                self.context.find_var(init[0])['value'] = new_val
            self.context.leave_context()
            Evaluator.trace_eval(lst, None)
            return None
        elif action == 'let':
            # (let ((a 11) (b 12) c) ...)
            var_bindings = lst[1]
            new_ctx = self.context.new_context()
            for var_binding in var_bindings:
                if is_type_list(var_binding):
                    assert len(var_binding) == 2, "must be initialization of var binding"
                    name = var_binding[0]
                    value = self.eval_form(var_binding[1])
                    self.context.add_var(name, value, new_ctx)
                else:
                    assert len(var_binding) == 1, "must be default initialization of var binding"
                    name = var_binding[0]
                    self.context.add_var(name, None, new_ctx)

            self.context.enter_context(new_ctx)
            ret_val = self.eval_block(lst[2:])
            self.context.leave_context()
            Evaluator.trace_eval(lst, ret_val)
            return ret_val
        elif action == 'setq':
            # (setq name "Zhao")
            exprs = lst[1:]
            if len(exprs) % 2 != 0:
                raise RuntimeError("expect even number of arguments")
            for i in range(0, len(exprs), 2):
                symbol = exprs[i]
                value = self.eval_form(exprs[i + 1])
                self.context.add_var(symbol, value)
            Evaluator.trace_eval(lst, None)
            return None
        elif action == 'cons':
            # (cons 1 (cons 2 (cons 3 nil)))
            first = self.eval_form(lst[1])
            second = self.eval_form(lst[2])
            value = [first]
            if second is not None:
                value += second
            Evaluator.trace_eval(lst, value)
            return value
        elif action == 'first':
            # (first '(1 2 4 5 6))
            value = self.eval_form(lst[1])
            if not is_type_list(value):
                raise RuntimeError("Expect an list as its argument")
            value = value[0]
            Evaluator.trace_eval(lst, value)
            return value
        elif action == 'rest':
            # (rest '(1 2 4 5 6))
            value = self.eval_form(lst[1])
            if not is_type_list(value):
                raise RuntimeError("Expect an list as its argument")
            value = value[1:]
            Evaluator.trace_eval(lst, value)
            return value
        elif action == 'list':
            # (list 1 2 3)
            value = []
            for expr in lst[1:]:
                value += [self.eval_form(expr)]
            Evaluator.trace_eval(lst, value)
            return value
        elif action == 'defun':
            # (def name () ...)
            func = self.context.add_func(lst[1], lst[2], lst[3:])
            Evaluator.trace_eval(lst, func)
            return func
        elif action == 'lambda':
            # (lambda () ...)
            func = self.context.add_func('<lambda>', lst[1], lst[2:])
            Evaluator.trace_eval(lst, func)
            return func
        elif action == 'go':
            # (go label)
            continuation = self.context.find_continuation(lst[1])
            if continuation is None:
                raise RuntimeError(f"Can not find target label {lst[1]}")
            self.context.set_ctl_flag(CtlFlag.GOTO)
            return continuation
        elif action == 'tagbody':
            # (tagbody label ... (go label))
            i = 1
            value = None
            self.context.enter_lexical_scope(lst)
            code = lst[1:]
            while i < len(code):
                if is_type_list(lst[i]):
                    val = self.eval_form(lst[i])
                if self.context.get_ctl_flag() == CtlFlag.GOTO:
                    code = val
                    i = 0
                    self.context.set_ctl_flag(CtlFlag.Normal)
                    continue
                if i == len(lst) - 1:
                    value = val
                i += 1
            self.context.leave_lexical_scope()
            Evaluator.trace_eval(lst, value)
            return value
        elif action == 'defmacro':
            raise RuntimeError("Macro should be already collected and expanded")
        else:
            # (foo ...)
            func = self.context.find_func(action)
            if func is not None:
                val = self.eval_call(func, lst)
                Evaluator.trace_eval(lst, val)
                return val

            var = self.context.find_var(action)
            if var is not None:
                # (setq fn '+)
                # (fn 1 2)
                var_as_fn = var['value']
                func = self.context.find_func(var_as_fn)
                if func is None:
                    raise RuntimeError(f"Call to undefined symbol {var['value']}")
                val = self.eval_call(func, lst)
                Evaluator.trace_eval(lst, val)
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
            if is_type_list(expr) and len(expr) > 0:
                if expr[0] == 'defmacro':
                    self.context.add_macro(expr[1], expr[2], expr[3:])
                    if ENABLE_DEBUG:
                        print(f"Find macro definition {expr[1]}")
                    del sexpr_list[idx]
                    continue
            idx += 1

    def find_macro(self, name):
        for macro in self.context.macros:
            if macro['name'] == name:
                return macro
        return None

    def expand_macro(self, sexpr_list):
        if not is_type_list(sexpr_list):
            return None
        for idx, sexpr in enumerate(sexpr_list):
            if is_type_list(sexpr) and len(sexpr) > 0:
                if type(sexpr[0]) is str:
                    macro = self.find_macro(sexpr[0])
                    if macro is not None:
                        expanded = self.eval_call(macro, sexpr, is_macro_call=True)
                        sexpr_list[idx] = expanded
                        continue
                self.expand_macro(sexpr)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("No source file")
        exit(-1)

    script_name = ""
    for arg in sys.argv:
        if arg == '--debug':
            ENABLE_DEBUG = True
        elif arg.endswith('.lisp'):
            script_name = arg

    src_dir = os.path.dirname(os.path.realpath(__file__))
    lst_stdlib = SyntaxParser.parse_file(src_dir + "/../stdlib/stdlib.lisp")
    lst_source = SyntaxParser.parse_file(script_name)
    e = Evaluator()
    e.eval_top_level(lst_stdlib)
    e.eval_top_level(lst_source)
