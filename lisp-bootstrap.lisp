(ell-mdef defmacro
  (ell-lam (defmacro-form)
    #`(ell-mdef ,(second defmacro-form)
        (ell-lam (macro-call-form)
          (apply-syntax-list
            (ell-lam ,(third defmacro-form)
              ,(fourth defmacro-form))
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
  #`(progn (ell-def ,name ,value) ',name))

(defmacro defvar (name &optional (value #'unspecified))
  #`(defparameter ,name (if (definedp ,name) ,name ,value)))

(defmacro defun/f (name function)
  #`(progn (ell-fdef ,name ,function) ',name))

(defmacro defun (name sig &rest body)
  #`(defun/f ,name (ell-lam ,sig (progn ,@body))))

(defmacro funcall (fun &rest args)
  #`(ell-app ,fun ,@args))

(defmacro function (name)
  #`(ell-fref ,name))

(defmacro setq (name value)
  #`(ell-set ,name ,value))

(defmacro fsetq (name value)
  #`(ell-fset ,name ,value))

(defmacro c-expression (&rest exprs)
  #`(ell-snip ,@exprs))

(defmacro c-statement (&rest exprs)
  #`(ell-stmt ,@exprs))

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
  #`(c-expression ,@snippets))

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

(defclass <condition>)
(defclass <unbound-variable> (<condition>)
  name)
(defclass <unbound-function> (<condition>)
  name)

(defclass <restart> (<condition>))
(defclass <use-value> (<restart>)
  value)

(defclass <handler>)

(defclass <default-handler> (<handler>))

(defclass <user-handler> (<handler>)
  condition-class
  handler-function
  next-handler)

(defmethod print-object ((h <handler>))
  (print-object '#<handler>))

(defun make-user-handler (condition-class handler-function next-handler)
  (let ((h (make <user-handler>)))
    (set-slot-value h 'condition-class condition-class)
    (set-slot-value h 'handler-function handler-function)
    (set-slot-value h 'next-handler next-handler)
    h))

(defparameter *current-handler* (make <default-handler>))

(defgeneric handle-condition (handler condition))

(defmethod handle-condition ((h <default-handler>) (c <condition>))
  (print condition)
  (exit))

(defmethod handle-condition ((h <user-handler>) (c <condition>))
  (if (handler-matches? h condition)
      (funcall (slot-value h 'handler-function)
               condition
               (lambda ()
                 (handle-condition (slot-value h 'next-handler) condition)))
      (handle-condition (slot-value h 'next-handler) condition)))

(defgeneric handler-matches? (user-handler condition))

(defmethod handler-matches? ((h <user-handler>) (c <condition>))
  (type? condition (slot-value h 'condition-class)))

(defun handler-bind/f (condition-class user-handler-function body-thunk)
  (let ((the-handler-function (lambda (condition call-next-handler)
                                (block resume
                                  (funcall user-handler-function
                                           condition
                                           (lambda (value) (return-from resume value)))
                                  (funcall call-next-handler)))))
    (fluid-let *current-handler* (make-user-handler condition-class
                                                    the-handler-function
                                                    *current-handler*)
      (funcall body-thunk))))

(defmacro handler-bind (condition-class user-handler-function &rest body)
  #`(handler-bind/f ,condition-class ,user-handler-function
                    (lambda () ,@body)))

(defun signal (condition)
  (handle-condition *current-handler* condition))

(defun warn (condition)
  (signal condition)
  (print condition))

(defun error (condition)
  (signal condition)
  (invoke-debugger condition))

(defun cerror (condition)
  (block use-value
    (handler-bind <use-value> (lambda (restart resume)
                                (return-from use-value (slot-value restart 'value)))
      (error condition))))
