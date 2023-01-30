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

INVALID_ATOM_PREFIX = "Invalid-"


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
            if type(item) is str and INVALID_ATOM_PREFIX in item:
                raise RuntimeError(f"Syntax error at {item}")

    def to_py_list(self, i):
        # i.e. ['(', '+', '1', '2', ')'] =-> [['+', '1', '2']]
        if self.forms[i] != '(':
            raise RuntimeError("Must start with (")
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
        if self.forms[k] != ')':
            raise RuntimeError("Must end with )")
        del self.forms[i:k]
        self.forms[i] = sub

    def process_quote(self, lst):
        # `(...) => [`, (...)] surround with list, note this should be unpacked
        # during evaluation time
        k = 0
        while k < len(lst):
            if type(lst[k]) is list:
                self.process_quote(lst[k])
            elif lst[k] == "'" or lst[k] == '`' or lst[k] == ',' or lst[k] == ',@':
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
            s = ''
            while i < len(self.text):
                ch = self.text[i]
                if cond(ch):
                    s += self.text[i]
                else:
                    break
                i += 1
            return s, i

        if self.text[idx].isalpha() or self.text[idx] in '&:':
            # symbol, &symbol, :symbol
            s, i = accept_if(idx, lambda c: ('a' <= c <= 'z') or ('A' <= c <= 'Z') or c in '-&:')
            if s == 't':
                return True, i
            elif s == 'nil':
                return None, i
            return s, i
        elif self.text[idx] in '()+*/%\'`,':
            # operator(+-*/%), `(this is ,a ,@b ), 'this
            if idx + 1 < len(self.text) and self.text[idx:idx + 2] == ',@':
                return ',@', idx + 2
            return self.text[idx], idx + 1
        elif self.text[idx] in '><=!':
            tk = self.text[idx]
            if self.text[idx + 1] == '=':
                tk += '='
                return tk, idx + 2
            return tk, idx + 1
        elif self.text[idx] == '\"':
            # string
            try:
                end = self.text.index('\"', idx + 1)
            except ValueError:
                raise RuntimeError("Invalid string literal")
            else:
                return self.text[idx:end + 1], end + 1
        elif self.text[idx].isnumeric() or self.text[idx] == '-':
            # minus(-) operator
            if self.text[idx] == '-':
                if idx + 1 < len(self.text) and (not self.text[idx + 1].isnumeric()):
                    return "-", idx + 1
            # 3.14, -3
            s, i = accept_if(idx, lambda c: c.isnumeric() or c == '.' or c == '-')
            return eval(s), i
        elif self.text[idx] == ' ':
            return " ", idx + 1
        return INVALID_ATOM_PREFIX + self.text[idx], idx + 1

    def make_form_list(self):
        # remove comment lines
        self.remove_comments()

        # source code to forms in a whole
        i = 0
        while i < len(self.text):
            lexeme, ni = self.make_atom(i)
            i = ni
            if lexeme != ' ':
                self.forms.append(lexeme)
        self.syntax_check()

        # process top-level forms
        i = 0
        while i < len(self.forms):
            if self.forms[i] == '(':
                self.to_py_list(i)
            i += 1

        # process quote after py-list was generated
        self.process_quote(self.forms)

        return self.forms
