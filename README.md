# Steed
<img src="asset/logo.png?raw=true" align="right" alt="" weight="120" height="120"/>Hello, this is a free and complete lisp implementation, most of my Lisp knowledge comes from [Practical Common List](https://gigamonkeys.com/book/) and [Successful Lisp(Primary)](https://dept-info.labri.fr/~strandh/Teaching/MTP/Common/David-Lamkins/contents.html),
and some old memories from SICP. All of these make up my impression on Lisp.
The main reference manual is Successful Lisp, which contains core design and core concepts of Lisp, I will re-design and implement the parts that do not suit my taste.

## Syntax
#### **Comments**
```
;; Happy 2023 Chinese New Year~
```
#### **Symbol** 
`1 3.4 -6 foo bar`
#### **Lambda** 
`(lambda () ...)`
#### **Function Definition** 
`(def name () ...)`
#### **Function Call** 
`(<func> ...)`
#### **Block**
`(block (..) (..) (..))`
#### **Let Binding** 
```
(let ((a 11)
       (b 12) 
       c) 
       ...)
```
#### **If** 
`(if cond (then-block) (else-block)? )`
#### **For** 
`(for (i 0) (< i 10) (+ i 1) (...))`
#### **Macro Definition** 
`(macro name () ...)`
#### **Set Value** 
`(setq name "Tang")`
#### **Quote**
```
(quote a)
'(a b)
`((+ 1 2) ,(+1 2))
```
#### **List Manipulation**
- `cons`
```
(cons 1 nil)
(cons 1 (cons 2 (cons 3 nil)))
```
The second argument must be a list or NIL.
- `list`
```
(list 1 2 3)
(list 1 2 'hello "there" 3)
```
- `first` `(first (list 1 2 4))`
- `rest` `(rest (list 1 2 4))`

## Standard library
#### rcons
```
(cons 1 '(2 3)) => [1, 2, 3]
(rcons '(2 3) 4) => [4, 2, 3]
```
Append element at the end of list, reverse version of `cons`

#### when
```
(when (== 3 (+ 1 2)) (...))
```
#### unless
```
(unless (== 3 (+ 1 2)) (...))
```