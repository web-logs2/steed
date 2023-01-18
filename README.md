# Steed
<img src="logo.png?raw=true" align="right" alt="" weight="120" height="120"/>Hello, this is a free and complete lisp implementation, most of my Lisp knowledge comes from [Practical Common List](https://gigamonkeys.com/book/) and [Successful Lisp](https://dept-info.labri.fr/~strandh/Teaching/MTP/Common/David-Lamkins/contents.html) books,
and some old memories. All of these make up my impression on Lisp.
The main reference manual is that book which contains core design and core concepts of Lisp, I will re-design and implement the parts that do not suit my taste.


### Syntax
- **Symbol** `1 3.4 -6 foo bar`
- **Lambda** `(lambda () ...)`
- **Function Definition** `(def name () ...)`
- **Function Call** `(<func> ...)`
- **Block** ` (block (..) (..) (..))`
- **Let Binding** `(let ((a 11) (b 12) c) ...)`
- **If** `(if cond (then-block) (else-block)? )`
- **For** `(for (i 0) (< i 10) (+ i 1) (...))`
- **Macro** `(macro name () ...)`
- **Quote**
```
(quote a)
'(a b)
`((+ 1 2) ,(+1 2))
```

# Progress
It works in progress as I'm read Practical Common List now, you can also contributed your awesome code if you are interested in it.
