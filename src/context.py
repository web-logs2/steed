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
from enum import Enum

from src.builtin import TheBuiltin
from src.utils import is_type_list

CtlFlag = Enum('CtlFlag', ['Normal', 'GOTO'])


class Context:
    def __init__(self):
        self.lexical_scope = None
        self.label_map = None
        self.contexts = [{"func": [func for func in TheBuiltin], "var": []}]
        self.macros = []
        self.ctl_flag = CtlFlag.Normal

    def get_ctl_flag(self):
        return self.ctl_flag

    def set_ctl_flag(self, flag):
        self.ctl_flag = flag

    def enter_lexical_scope(self, scope):
        self.lexical_scope = scope
        self.label_map = {}

    def find_continuation(self, label):
        for i in range(len(self.lexical_scope)):
            if self.lexical_scope[i] == label:
                return self.lexical_scope[i + 1:]
        return None

    def leave_lexical_scope(self):
        self.lexical_scope = None
        self.label_map = None

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

    def add_var(self, name, value, ctx=None):
        var = {"name": name, "value": value}
        if ctx is None:
            self.contexts[-1]['var'].append(var)
        else:
            ctx['var'].append(var)

    def add_func(self, name, param, body, ctx=None):
        func = {'name': name, 'param': param, 'body': body}
        if ctx is None:
            self.contexts[-1]['func'].append(func)
        else:
            ctx['func'].append(func)
        return func

    def add_macro(self, name, param, body):
        macro = {'name': name, 'param': param, 'body': body}
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
