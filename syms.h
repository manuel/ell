/**** Special forms understood by the compiler: ****/

/* (ell-fref function-name) -> function ;; lookup in function namespace */
ELL_DEFSYM(core_fref, "ell-fref")
/* (ell-def name value) ;; define global variable */
ELL_DEFSYM(core_def,  "ell-def")
/* (ell-fdef name value) ;; define global function */
ELL_DEFSYM(core_fdef, "ell-fdef")
/* (ell-defp name) -> boolean ;; check whether global variable is defined */
ELL_DEFSYM(core_defp, "ell-defp")
/* (ell-fdefp name) -> boolean ;; check whether global functions is defined */
ELL_DEFSYM(core_fdefp,"ell-fdefp")
/* (ell-set name value) ;; update global or lexical variable */
ELL_DEFSYM(core_set,  "ell-set")
/* (ell-fset name value) ;; update global or lexical function */
ELL_DEFSYM(core_fset, "ell-fset")
/* (ell-cond test then else) -> result ;; perform then or else branch depending on test */
ELL_DEFSYM(core_cond, "ell-cond")
/* (ell-seq &rest exprs) -> result ;; performs expressions sequentially, return result of last */
ELL_DEFSYM(core_seq,  "ell-seq")
/* (ell-lam params body) -> function ;; create anonymous function */
ELL_DEFSYM(core_lam,  "ell-lam")
/* (ell-app op &rest args) -> result ;; apply function to arguments */
ELL_DEFSYM(core_app,  "ell-app")
/* (ell-loop expr) ;; infinite loop */
ELL_DEFSYM(core_loop, "ell-loop")
/* (ell-mdef name expander) ;; define macro expander function, that takes and returns syntax */
ELL_DEFSYM(core_mdef, "ell-mdef")

/* Data and syntax quotation: */
ELL_DEFSYM(core_quote, "quote")
ELL_DEFSYM(core_syntax, "syntax")
ELL_DEFSYM(core_quasisyntax, "quasisyntax")
ELL_DEFSYM(core_unsyntax, "unsyntax")
ELL_DEFSYM(core_unsyntax_splicing, "unsyntax-splicing")

/**** Built-in functions: ****/

ELL_DEFSYM(add, "add")
ELL_DEFSYM(first, "first")
ELL_DEFSYM(second, "second")
ELL_DEFSYM(third, "third")
ELL_DEFSYM(fourth, "fourth")
ELL_DEFSYM(print_object, "print-object")
ELL_DEFSYM(all, "all")
ELL_DEFSYM(front, "front")
ELL_DEFSYM(emptyp, "emptyp")
ELL_DEFSYM(pop_front, "pop-front")
ELL_DEFSYM(core_send, "send")
ELL_DEFSYM(core_syntax_list, "syntax-list")
ELL_DEFSYM(core_append_syntax_lists, "append-syntax-lists")
ELL_DEFSYM(core_apply_syntax_list, "apply-syntax-list")
ELL_DEFSYM(default_handle, "default-handle")

/* Note that there are additional built-in functions defined in
   `ellrt,c' that are not listed here, which is a documentation bug. */

/**** Defined symbols: ****/

ELL_DEFSYM(param_optional, "&optional")
ELL_DEFSYM(param_key, "&key")
ELL_DEFSYM(param_rest, "&rest")
ELL_DEFSYM(param_all_keys, "&all-keys")
