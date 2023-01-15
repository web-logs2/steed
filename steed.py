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
import os
import re
import string
import sys

TheBuiltin = [
    {'name': "format", 'param': ['fmt', "args"], 'body': lambda ctxs, args: print(args['fmt'].format(args['args']))},
    {'name': "+", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] + args['b']},
    {'name': "-", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] - args['b']},
    {'name': "*", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] * args['b']},
    {'name': "/", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] / args['b']},
    {'name': "&", 'param': ['a', "b"], 'body': lambda ctxs, args: args['a'] % args['b']},
]


def log_trace(a, b):
    print(f"== Evaluate S-expression {a} that produces {b}({type(b)})")


def make_sexpr(text):
    """
    convert source code to s-expressions and make s-expressions to python
    objects to facilitate further use

    :param text: source code
    :return: s-expression list
    """
    text = text.replace('\n', '')

    def unit(idx):
        def accept_if(i, cond):
            s = text[i]
            while i + 1 < len(text):
                i += 1
                ch = text[i]
                if cond(ch):
                    s += text[i]
                else:
                    break
            return s, i

        if text[idx].isalpha():
            # identifiers
            return accept_if(idx, lambda c: ('a' <= c <= 'z') or ('A' <= c <= 'Z') or c == '-')
        if text[idx] in '()+*/%\'':
            # operators
            return text[idx], i + 1
        elif text[idx] == '\"':
            # string
            end = text.index('\"', idx + 1)
            return text[idx:end + 1], end + 1
        elif text[idx].isnumeric() or text[idx] == '-':
            # minus(-) operator
            if text[idx] == '-':
                if idx + 1 < len(text) and (not text[idx + 1].isnumeric()):
                    return "-", idx + 1
            # 3.14, -3
            return accept_if(idx, lambda c: c.isnumeric() or c == '.')
        elif text[idx] == ' ':
            return " ", idx + 1
        return "Invalid-" + text[idx], idx + 1

    i = 0
    sexprs = []
    while i < len(text):
        lexeme, ni = unit(i)
        i = ni
        if lexeme != ' ':
            sexprs.append(lexeme)
    invalid = [item.replace('Invalid-', '') for item in sexprs if 'Invalid-' in item]
    if len(invalid) > 0:
        print("SyntaxError: Found invalid content " + str(invalid))
        exit(-1)

    # i.e. ['(', '+', '1', '2', ')'] =-> [['+', '1', '2']]
    def to_py_list(i):
        if sexprs[i] != '(':
            raise ValueError("Must start with (")
        sub = []
        k = i + 1
        while k < len(sexprs):
            if sexprs[k] == '(':
                to_py_list(k)
            if sexprs[k] != ')':
                sub.append(sexprs[k])
            else:
                break
            k += 1
        if sexprs[k] != ')':
            raise ValueError("Must close with )")
        # delete elements i range of [), so we can replace these elements with sub
        del sexprs[i:k]
        sexprs[i] = sub

    # process top-level s-expressions
    i = 0
    while i < len(sexprs):
        if sexprs[i] == '(':
            to_py_list(i)
        i += 1
    return sexprs


def eval_sexpr_list(contexts, sexprs):
    def eval_sexpr(sexpr):
        if re.match("\".*\"", sexprs) or re.match("-?[0-9][0-9.]]*", sexprs):
            return eval(sexprs)
        else:
            for var in contexts[-1]['var']:
                if var['name'] == sexprs:
                    return var['value']
        raise ValueError(f"Unknown single s-expression {sexprs}")

    if type(sexprs) is not list:
        val = eval_sexpr(sexprs)
        log_trace(sexprs, val)
        return val

    if len(sexprs) == 0:
        log_trace(sexprs, None)
        return None

    action = sexprs[0]
    if action == 'quote' or action == '\'':
        # quote
        log_trace(sexprs, sexprs)
        return sexprs
    elif action == 'def':
        # function definition
        func = {'name': sexprs[1], 'param': sexprs[2], 'body': sexprs[3:]}
        contexts[-1]['func'].append(func)
        log_trace(sexprs, func)
        return func
    else:
        def find_func(ctxs, name):
            for ctx_idx in reversed(range(len(ctxs))):
                for func in contexts[ctx_idx]['func']:
                    if func['name'] == action:
                        return func
            return None

        # general identifier, either a use of var or function call
        for var in contexts[-1]['var']:
            if var['name'] == action:
                log_trace(sexprs, var['value'])
                return var['value']

        func = find_func(contexts, action)
        if func is not None:
            # Prepare arguments
            new_ctx = {"func": [], "var": []}

            if len(func['param']) != len(sexprs) - 1:
                raise ValueError(
                    f"Function {func['name']} expects {len(func['param'])} parameter but found {len(sexprs) - 1}")

            for i in range(0, len(func['param'])):
                new_ctx['var'].append({"name": func['param'][i], "value": eval_sexpr_list(contexts, sexprs[i + 1])})

            # Call builtin or user defined function
            body = func['body']
            ret_val = None
            contexts.append(new_ctx)
            if callable(body):
                args = {}
                for item in new_ctx['var']:
                    args[item['name']] = item['value']
                ret_val = body(contexts, args)
            else:
                for i in range(len(body)):
                    var = eval_sexpr_list(contexts, body[i])
                if i == len(body) - 1:
                    ret_val = var
            del contexts[-1]

            log_trace(sexprs, ret_val)
            return ret_val
    raise ValueError("Should not reach here")


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("No source file")
        exit(-1)

    with open(sys.argv[1]) as file:
        content = file.read()
        sexprs = make_sexpr(content)

    # register builtin functions
    contexts = [{"func": [func for func in TheBuiltin], "var": []}]

    # evaluate top-level expressions
    for i in sexprs:
        val = eval_sexpr_list(contexts, i)
