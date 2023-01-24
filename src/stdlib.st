(macro when (cond body)
    `(if ,cond (block ,body))
)

(macro unless (cond body)
    `(if (not ,cond) (block ,body))
)

(macro dotimes (name limit body)
    `(for (,name 0) (< ,name ,limit) (+ ,name 1) ,body)
)