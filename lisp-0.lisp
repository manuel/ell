(ell-mdef defmacro
  (ell-lam (defmacro-form)
    #`(ell-mdef ,(send defmacro-form (ell-fref second))
        (ell-lam (macro-call-form)
          (apply-syntax-list
            (ell-lam ,(send defmacro-form (ell-fref third))
              ,(send defmacro-form (ell-fref fourth)) #<function>)
            (syntax-list-rest macro-call-form)) #<function>)) #<function>))

(defmacro defsyntax (name expander)
  #`(ell-mdef ,name ,expander))

(defmacro progn (&rest exprs)
  #`(ell-seq ,@exprs))

(defmacro lambda (sig &rest body)
  #`(ell-lam ,sig (progn ,@body) #<function>))

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
  #`(funcall (lambda (,@(map-list (lambda (binding) (send binding (function first))) bindings))
               ,@body)
             ,@(map-list (lambda (binding) (send binding (function second))) bindings)))

(defmacro do (vars test &rest body)
  #`(let ,(map-list (lambda (var)
                      #`(,(send var (function first))
                         ,(send var (function second))))
                    vars)
      (while ,test
        ,@body
        ,@(map-list (lambda (var)
                      #`(setq ,(send var (function first))
                              ,(send var (function third))))
                    vars))))

(defmacro defclass (name &optional (superclasses #'()) &rest slot-specs)
  #`(progn
      (defvar ,name (make-class ',name))
      (add-superclass ,name <object>)
      ,@(map-list (lambda (superclass)
                    #`(add-superclass ,name ,superclass))
                  superclasses)
      ',name))

(defmacro defgeneric (name &optional params)
  #`(defun/f ,name (if (fdefinedp ,name)
                       (function ,name)
                       (make-generic-function ',name))))

(defmacro defmethod (name params &rest body)
  #`(progn
      (defgeneric ,name)
      (put-method (function ,name)
                  (lambda ,params ,@body)
                  ,@(dissect-generic-function-params params))
      ',name))

(defgeneric print-object)

(defmethod print-object ((o <object>))
  (print-object "#<object>"))

(defun print (object) (print-object object))

(defmacro c (&rest snippets)
  #`(c-expression ,snippets))

(defmacro fluid-let (name value &rest body)
  #`(let ((tmp ,name))
      (setq ,name ,value)
      (unwind-protect (progn ,@body)
        (setq ,name tmp))))

(defclass <string>)
(defclass <symbol>)
(defclass <integer>)
(defclass <boolean>)
(defclass <function>)
(defclass <unspecified>)
(defclass <linked-list>)
(defclass <linked-list-range>)
(defclass <syntax-list>)
(defclass <syntax-symbol>)
(defclass <syntax-string>)
(defclass <syntax-number>)
