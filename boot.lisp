(ell-mdef defmacro
  (ell-lam (defmacro-form)
    #`(ell-mdef ,(send defmacro-form 'second)
        (ell-lam (macro-call-form)
          (apply-syntax-list
            (ell-lam ,(send defmacro-form 'third)
              ,(send defmacro-form 'fourth))
            (syntax-list-rest macro-call-form))))))

(defmacro progn (&rest exprs)
  #`(ell-seq ,@exprs))

(defmacro lambda (sig &rest body)
  #`(ell-lam ,sig (progn ,@body)))

(defmacro if (test then &optional (else #'unspecified))
  #`(ell-cond ,test ,then ,else))

(defmacro when (test &rest body)
  #`(if ,test (progn ,@body)))

(defmacro not (x)
  #`(if ,x #f #t))

(defmacro unless (test &rest body)
  #`(when (not ,test) ,@body))

(defmacro definedp (name)
  #`(ell-defp ,name))

(defmacro fdefinedp (name)
  #`(ell-fdefp ,name))

(defmacro defparameter (name value)
  #`(ell-def ,name ,value))

(defmacro defvar (name value)
  #`(unless (definedp ,name)
      (defparameter ,name ,value)))

(defmacro define (name function)
  #`(ell-fdef ,name ,function))

(defmacro defun (name sig &rest body)
  #`(define ,name (lambda ,sig ,@body)))
