(ell-mdef defmacro
  (ell-lam (defmacro-form)
    #`(ell-mdef ,(send defmacro-form 'second)
        (ell-lam (macro-call-form)
          (apply-syntax-list
            (ell-lam ,(send defmacro-form 'third)
              ,(send defmacro-form 'fourth))
            (syntax-list-rest macro-call-form))))))

(defmacro defsyntax (name expander)
  #`(ell-mdef ,name ,expander))

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

(defmacro function (name)
  #`(ell-fref ,name))

(defmacro setq (name value)
  #`(ell-set ,name ,value))

(defmacro fsetq (name value)
  #`(ell-fset ,name ,value))

(defmacro block (label &rest body)
  #`(block/f (lambda (,label) ,@body)))

(defmacro return-from (label &optional (value #'unspecified))
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

(defmacro let (bindings &rest body)
  #`(funcall (lambda (,@(map-list (lambda (binding) (send binding 'first)) bindings))
               ,@body)
             ,@(map-list (lambda (binding) (send binding 'second)) bindings)))

(defmacro defclass (name &optional (superclasses #'()))
  #`(progn
      (defvar ,name (make-class))
      ,@(map-list (lambda (superclass)
                    #`(add-superclass ,name ,superclass))
                  superclasses)
      unspecified))

(defmacro defgeneric (name &optional params)
  #`(defun ,name (&rest args)
      (let ((receiver (send (send args 'all) 'front)))
        (funcall (find-method receiver ',name) args))))

(defmacro defmethod (name params &rest body)
  #`(progn
      (defgeneric ,name)
      (put-method ,(send (send params 'first) 'second) 
                  ',name
                  (lambda ,params ,@body))
      unspecified))
