(defpackage Generic-Lisp ((Generic-Lisp-Kernel K))
  (signature
    (include K::<object>
             K::<class>)
    (defmacro let)
    (defmacro let*)
    (defmacro set)
    (defmacro def)
    
    (defmacro defun)
    (defmacro lambda)
    (defmacro funcall)
    (defmacro apply)
    
    (defmacro if)
    (defmacro when)
    (defmacro unless)
    (defmacro block)
    (defmacro return-from)
    (defmacro return)
    (defmacro unwind-protect)

    (defun eq)
    (defun make-instance)
    (defmacro slot-value)
    (defmacro set-slot-value)
    (defun class-of)
    
    (defmacro defclass)
    (defmacro defgeneric)
    (defmacro defmethod)
    (defun subclassp)

    (defmacro defmixin)
    (defmacro definstance)
    
    (defmacro defpackage)
    (defmacro signature)
    (defmacro structure)
    (defmacro include)

    (defun signal)
    (defun throw)
    (defmacro handle)
    (defmacro catch)
  )
)
