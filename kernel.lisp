;;;; Core types
(deftype <object>)
(deftype <class>)
(deftype <symbol>)
(deftype <function>)
(deftype <generic-function> <: <function>)
(deftype <method> <: <function>)
(deftype <condition>)
(deftype <restart> <: <condition>)
(deftype <boolean>)
(deftype <nil>)
(defconst #t)
(defconst #f)
(defconst nil)
;;;; Bindings
(defmacro def (name value -> <object>))
(defmacro set (var-or-fun-name value -> <object>))
(defun boundp (var-or-fun-sym -> <boolean>))
;;;; Functions
(defmacro function (name -> <function>))
(defmacro lambda (signature <form> -> <function>))
(defun funcall (sym-or-fun args key-args -> <object>))
(defun apply (sym-or-fun &rest &all-keys -> <object>))
;;;; Control flow
(defmacro if (<boolean> <form> <form> -> <object>))
(defmacro progn (&body forms -> <object>))
(defmacro block (<label> <form> -> <object>))
(defmacro return-from (<label> &optional <object>))
(defmacro unwind-protect (protected cleanup))
(defmacro tagbody (&rest body))
(defmacro go (tag))
;;;; Macros
(defmacro defmacro (name signature &body body))
(defmacro eval-when-compile (form))
;;;; Quotations
(defmacro quote (<form> -> <form>))
(defmacro quasiquote (<form> -> <form>))
(defmacro unquote (<form>))
(defmacro unquote-splicing (<list-form>))
(defun datum->syntax (<form> <template-identifier> -> <form>)) ; &whole
;;;; Objects
(defun make-instance (symbol-or-class &all-keys slot-initializers))
(defun call-method (<object> <symbol> &rest &all-keys))
(defun slot-value (<object> <symbol> -> <object>))
(defun set-slot-value (<object> <symbol> <object>))
(defun class-of (<object> -> <class>))
(defun eq (<object> <object> -> <boolean>))
;;;; Classes
(defun make-class (<symbol> -> <class>))
(defun set-slot-specs (<class> ...))
(defun set-superclass (<class> superclass))
(defun set-mixins (<class> superclass))
(defun set-method (<class> <symbol> <function>))
(defun subclassp (<class> <class> -> <boolean>))
;;;; Exceptions
(defmacro handler-bind (handler-bindings &body forms -> <object>))
(defun signal (<condition> -> <object>))
;;;; Evaluation
(defun eval (<form> env -> <object>))
;;;; OS
(defmacro native ((c-snippet <string>)))
;;;; Packages
(package name exports form)
(import name)


(deftype <collection>)
(defun all (<collection> -> <range>))

(deftype <range>)
(defun elt (<range> <number> -> <object>))
(defun subseq (<range> <number> &optional <number> -> <range>))

(deftype <form>)
(deftype <symbol-form> <: <form>)
(deftype <compound-form> <: (<form> <collection>))
(deftype <literal-form> <: <form>)
