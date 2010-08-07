(ell-mdef defmacro
  (ell-lam (defmacro-form)
    #`(ell-mdef ,(send defmacro-form 'second)
        (ell-lam (macro-call-form)
          (apply-syntax-list
            (ell-lam ,(send defmacro-form 'third)
              ,(send defmacro-form 'fourth) anonymous)
            (syntax-list-rest macro-call-form)) anonymous)) anonymous))

(defmacro defsyntax (name expander)
  #`(ell-mdef ,name ,expander))

(defmacro progn (&rest exprs)
  #`(ell-seq ,@exprs))

(defmacro lambda (sig &rest body)
  #`(ell-lam ,sig (progn ,@body) anonymous))

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
  #`(progn (ell-def ,name ,value) ',name))

(defmacro defvar (name &optional (value #'unspecified))
  #`(defparameter ,name (if (definedp ,name) ,name ,value)))

(defmacro defun/f (name function)
  #`(progn (ell-fdef ,name ,function) ',name))

(defmacro defun (name sig &rest body)
  #`(defun/f ,name (ell-lam ,sig (progn ,@body) ,name)))

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
         (if ,test 
             (progn ,@body)
             (return-from exit unspecified)))))

(defmacro until (test &rest body)
  #`(while (not ,test) ,@body))

(defmacro let (bindings &rest body)
  #`(funcall (lambda (,@(map-list (lambda (binding) (send binding 'first)) bindings))
               ,@body)
             ,@(map-list (lambda (binding) (send binding 'second)) bindings)))

(defmacro do (vars test &rest body)
  #`(let ,(map-list (lambda (var)
                      #`(,(send var 'first)
                         ,(send var 'second)))
                    vars)
      (while ,test
        ,@body
        ,@(map-list (lambda (var)
                      #`(setq ,(send var 'first) 
                              ,(send var 'third)))
                    vars))))

(defmacro defclass (name &optional (superclasses #'()) &rest slot-specs)
  #`(progn
      (defvar ,name (make-class))
      ,@(map-list (lambda (superclass)
                    #`(add-superclass ,name ,superclass))
                  superclasses)
      ',name))

(defmacro defgeneric (name &optional params)
  #`(defun ,name (&rest args)
      (let ((receiver (send (send args 'all) 'front)))
        (apply (find-method receiver ',name) args))))

(defmacro defmethod (name params &rest body)
  #`(progn
      (defgeneric ,name)
      (put-method ,(send (send params 'first) 'second) 
                  ',name
                  (lambda ,params ,@body))
      ',name))

(defgeneric print-object (object))
(defun print (object) (print-object object))

(defmacro c (&rest snippets)
  #`(c-expression ,snippets))
