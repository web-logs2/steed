import re

t = """
(def hello-world ()
    (format t "hello world!")
    (+ 7.99 -4)
    (- 3 (* -5.7 6))
)
"""


def make_s_expr(text):
    def unit(idx):
        is_name = lambda c: ('a' <= c <= 'z') or ('A' <= c <= 'Z') or c == '-'
        if text[idx].isalpha():
            t = text[idx]
            while idx + 1 < len(text):
                idx += 1
                if is_name(text[i]):
                    t += text[idx]
            return t, idx
        if text[idx] in '()+*/%\'':
            return "(", i + 1
        elif text[idx].isnumeric() or text[idx] == '-':
            t = text[idx]
            if t == '-':
                if idx + 1 < len(text) and (not text[idx + 1].isnumeric()):
                    return "-", idx + 1
            # 3.14, -3
            while idx + 1 < len(text):
                idx += 1
                if text[idx].isnumeric():
                    t += text[idx]
                elif text[idx] == '.':
                    t += '.'
                else:
                    break
            return t, idx
        return "Invalid", idx + 1

    i = 0
    while i < len(text):
        lexeme, ni = unit(i)
        print(lexeme)
        i = ni


if __name__ == '__main__':
    make_s_expr(t)
