(macro when (cond body)
    `(if ,cond (block ,body))
)

(macro unless (cond body)
    `(if (not ,cond) (block ,body))
)