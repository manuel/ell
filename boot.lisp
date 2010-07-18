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
  #`(defparameter ,name (if (definedp ,name) ,name ,value)))

(defmacro defun/f (name function)
  #`(ell-fdef ,name ,function))

(defmacro defun (name sig &rest body)
  #`(defun/f ,name (lambda ,sig ,@body)))

(defmacro funcall (fun &rest args)
  #`(ell-app ,fun ,@args))

(defmacro block (label &rest body)
  #`(block/f (lambda (,label) ,@body)))

(defmacro return-from (label value)
  #`(funcall ,label ,value))

(defmacro unwind-protect (protected &rest cleanups)
  #`(unwind-protect/f (lambda () ,protected)
                      (lambda () ,@cleanups)))

(defmacro loop (&rest exprs)
  #`(ell-loop (progn ,@exprs)))

(defmacro while (test &rest body)
  #`(block exit
      (loop
         (unless ,test (return-from exit unspecified))
         ,@body)))
