# Steed
<img src="logo.png?raw=true" align="right" alt="" weight="120" height="120"/>Hello, this is a free and complete lisp implementation, most of my Lisp knowledge comes from [Practical Common List](https://gigamonkeys.com/book/) book,
and some old memories. All of these make up my impression on Lisp.
The main reference manual is that book which contains core design and core concepts of Lisp, I will re-design and implement the parts that do not suit my taste.


# Syntax
## Function
The skeleton of a function definitio looks like this:
```
(def name (param*)
  body-form*)
```
Where `*` means you can create sequential ones.

## Lambda
When it seems like overkill to define a new function with DEFUN, you can create an "anonymous" function using a lambda expression
```
((lambda ( x y) (+ x y)) 2 3)
```
You can imagine that *Lambda* is name of this anonymous function, it works exactly the same as normal function.
# Progress
It works in progress as I'm read Practical Common List now, you can also contributed your awesome code if you are interested in it.