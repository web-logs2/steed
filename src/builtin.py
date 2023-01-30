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
TheBuiltin = [
    {'name': "format", 'param': ['fmt', "args"], 'body': lambda args: print(args['fmt'].format(args['args']))},
    {'name': "assert", 'param': ['cond', "msg"], 'body': lambda args: steed_asert(args)},
    {'name': "+", 'param': ['a', "b"], 'body': lambda args: args['a'] + args['b']},
    {'name': "-", 'param': ['a', "b"], 'body': lambda args: args['a'] - args['b']},
    {'name': "*", 'param': ['a', "b"], 'body': lambda args: args['a'] * args['b']},
    {'name': "/", 'param': ['a', "b"], 'body': lambda args: args['a'] / args['b']},
    {'name': "&", 'param': ['a', "b"], 'body': lambda args: args['a'] % args['b']},
    {'name': ">", 'param': ['a', "b"], 'body': lambda args: True if args['a'] > args['b'] else None},
    {'name': ">=", 'param': ['a', "b"], 'body': lambda args: True if args['a'] >= args['b'] else None},
    {'name': "<", 'param': ['a', "b"], 'body': lambda args: True if args['a'] < args['b'] else None},
    {'name': "<=", 'param': ['a', "b"], 'body': lambda args: True if args['a'] <= args['b'] else None},
    {'name': "!=", 'param': ['a', "b"], 'body': lambda args: True if args['a'] != args['b'] else None},
    {'name': "==", 'param': ['a', "b"], 'body': lambda args: True if args['a'] == args['b'] else None},
    {'name': "and", 'param': ['a', "b"], 'body': lambda args: True if args['a'] and args['b'] else None},
    {'name': "or", 'param': ['a', "b"], 'body': lambda args: True if args['a'] or args['b'] else None},
    {'name': "not", 'param': ['a'], 'body': lambda args: True if not args['a'] else None},
    {'name': "eq", 'param': ['a', 'b'], 'body': lambda args: True if args['a'] == args['b'] else None},
]


def steed_asert(args):
    if not args['cond']:
        raise RuntimeError(args['msg'])
