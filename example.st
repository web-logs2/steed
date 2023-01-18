(def concate (a b)
 (format "{}" a )
)
(concate "hello" "world")
(assert
    (==
        ((lambda ( x y) (+ x y)) 2 3)
        5
    )
    "why not"
)

(let ((a 7) (b 2) c)
    (format "{}" a)
    (format "{}" b)
    (let ((c 4) (b 11))
        (format "{}" c)
        (format "{}" b)
    )
)

(if true ( format "{}" "then-block"))
(if true ( format "{}" "then-block") (format "{}" "else-block") )
(if false ( format "{}" "then-block") (format "{}" "else-block"))
(if false ( format "{}" "then-block"))

(format "{}" (block (+ 1 1) (- 1 1) (* 9 9)))

(format "{}" '(1 2 3))
(format "{}" '3)
(format "{}" `(4 5))
(format "{}" `(4 5 (7 8 ,(+ 6 4) ,120)))

(format "{}" (when true (+1 2)) )
(format "{}" (when false (+1 2)) )

(assert (<= 3 5) "yes")
(assert (== (>=4 99) false) "yes")
(assert (!= 3 5) "yes")

(for (i 0) (< i 10) (+ i 1)
    (block (format "{}" i)
    (assert (< i 10) "why not")
    )
)
(dotimes i 100 (format "{}" i))