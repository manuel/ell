(kernel:define-syntax defmacro
  (kernel:lambda (defmacro-form)
    #`(kernel:define-syntax ,(kernel:nth defmacro-form 1)
        (kernel:lambda (macro-call-form)
          (kernel:apply
            (kernel:lambda ,(kernel:nth defmacro-form 2)
              ,(kernel:subrange defmacro-form 3))
            (kernel:subrange macro-call-form 1)))
        ,(kernel:nth defmacro-form 1))))

(defmacro defsyntax (name expander)
  #`(progn
      (kernel:define-syntax ,name ,expander)
      ,name))

(defmacro progn (&rest exprs)
  #`(kernel:begin ,@exprs))

(defmacro lambda (sig &rest body)
  #`(kernel:lambda ,sig (progn ,@body)))

(defmacro if (test then &optional (else #'void))
  #`(kernel:if ,test ,then ,else))

(defmacro when (test &rest body)
  #`(if ,test (progn ,@body)))

(defmacro not (x)
  #`(if ,x #f #t))

(defmacro unless (test &rest body)
  #`(when (not ,test) ,@body))

(defmacro definedp (name)
  #`(kernel:definedp ,name))

(defmacro fdefinedp (name)
  #`(kernel:fdefinedp ,name))

(defmacro defparameter (name value)
  #`(kernel:define ,name ,value))

(defmacro defvar (name value)
  #`(defparameter ,name (if (definedp ,name) ,name ,value)))

(defmacro defun/f (name function)
  #`(kernel:fdefine ,name ,function))

(defmacro defun (name sig &rest body)
  #`(defun/f ,name (lambda ,sig ,@body)))

(defmacro funcall (fun &rest args)
  #`(kernel:funcall ,fun ,@args))

(defmacro function (name)
  #`(kernel:function ,name))

(defmacro setq (name value)
  #`(kernel:set ,name ,value))

(defmacro fsetq (name value)
  #`(kernel:fset ,name ,value))

(defmacro block (label &rest body)
  #`(kernel:block/f (lambda (,label) ,@body)))

(defmacro return-from (label &optional (value #'unspecified))
  #`(funcall ,label ,value))

(defmacro unwind-protect (protected &rest cleanups)
  #`(kernel:unwind-protect/f (lambda () ,protected)
                             (lambda () ,@cleanups)))

(defmacro loop (&rest exprs)
  #`(kernel:loop (progn ,@exprs)))

(defmacro while (test &rest body)
  #`(block exit
      (loop
         (unless ,test (return-from exit void))
         ,@body)))

(defmacro let (bindings &rest body)
  #`(funcall (lambda (,@(kernel:map-list (lambda (binding) (kernel:send binding 'first)) bindings))
               ,@body)
             ,@(kernel:map-list (lambda (binding) (kernel:send binding 'second)) bindings)))

(defmacro prog1 (expr &rest exprs)
  #`(let ((tmp ,expr))
      ,@exprs
      tmp))

(defmacro prog2 (expr &rest exprs)
  #`(progn
      ,expr1
      (prog1 ,@exprs)))

(defmacro defclass (name &optional (superclasses #'()) &rest slot-specs)
  #`(progn
      (defvar ,name (kernel:make-class))
      ,@(kernel:map-list (lambda (superclass)
                           #`(kernel:add-superclass ,name ,superclass))
                         superclasses)
      ,name))

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
      ,name))

(defgeneric print-object (object))

