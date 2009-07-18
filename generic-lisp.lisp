(defpackage Generic-Lisp ((Generic-Lisp-Kernel K))
  (signature
    (include-all-variables K)
    (include-function
      K::apply
      K::boundp
      K::call-method
      K::call-with-escape-continuation
      K::class-of
      K::datum->syntax-object
      K::elt
      K::fboundp
      K::first
      K::funcall
      K::rest
      K::set-slot-value
      K::slot-value
      K::sub-class-p)
    (include-macro 
      K::defparameter
      K::eval-when-compile
      K::function
      K::let-macro
      K::progn
      K::quasiquote
      K::quote
      K::setq)
    (defmacro defclass)
    (defmacro defgeneric)
    (defmacro defmethod)
    (defmacro defvar)
    (defmacro lambda)
    (defmacro unwind-protect)
  )
)

(defpackage Generic-Lisp-Impl ()
  (structure
    (defmacro lambda (signature &body body)
      `(K::lambda ,signature (progn ,@body)))
    (defmacro unwind-protect (protected &body cleanups)
      `(K::unwind-protect ,protected (progn ,@cleanups)))
  )
)

(defpackage Make-Generic-Lisp ((Generic-Lisp-Kernel K) -> Generic-Lisp)
  (Generic-Lisp-Impl K))
