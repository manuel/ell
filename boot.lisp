(ell-mdef defmacro
  (ell-lam (defmacro-form)
    #`(ell-mdef ,(send defmacro-form 'second)
        (ell-lam (macro-call-form)
          (apply-syntax-list
            (ell-lam ,(send defmacro-form 'third)
              ,(send defmacro-form 'fourth))
            (syntax-list-rest macro-call-form))))))
