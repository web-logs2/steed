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

from src.steed import SyntaxParser, Evaluator

print("\033[1mSteed - an easy-to-use dialect of lisp\033[0m")

src_dir = os.path.dirname(os.path.realpath(__file__))
lst_stdlib = SyntaxParser.parse_file(src_dir + "/stdlib.st")
e = Evaluator()

while True:
    line = input("> ")
    try:
        lst = SyntaxParser.parse_text(line)
        val = e.eval_form(lst[0])
        print(val)
    except RuntimeError as ex:
        print(f"RuntimeError: {ex}")


