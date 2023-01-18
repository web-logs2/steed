(macro when (cond body)
    `(if ,cond (block ,body))
)