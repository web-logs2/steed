(defmacro when (cond body)
    `(if ,cond (block ,body))
)

(defmacro unless (cond body)
    `(if (not ,cond) (block ,body))
)

(defmacro dotimes (name limit body)
    `(for (,name 0) (< ,name ,limit) (+ ,name 1) ,body)
)

(defmacro rcons (rest first)
     `(cons ,first ,rest)
)