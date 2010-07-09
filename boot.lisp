(ell-mdef defmacro
  (ell-lam (defmacro-form)
    (quasisyntax 
      (ell-mdef (unsyntax (send defmacro-form (quote second)))
        (ell-lam (form)
          (apply-syntax-list
            (ell-lam (unsyntax (send defmacro-form (quote third)))
              (unsyntax (send defmacro-form (quote fourth))))
            (syntax-list-rest form)))))))
