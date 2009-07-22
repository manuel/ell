(defpackage Generic-Lisp-Kernel
  (signature
    ;; Built-in classes and objects
    (defclass <object>)
    (defclass <class>)
    (defclass <function>)
    (defclass <generic> <: <function>)
    (defclass <method> <: <function>)
    (defclass <condition>)
    (defclass <restart> <: <condition>)
    (defclass <boolean>)
    (defclass <nil>)
    (defvar true)
    (defvar false)
    (defvar nil)
    (defclass <form>)
    (defclass <symbol-form> <: <form>)
    (defclass <list-form> <: <form>)
    (defclass <literal-form> <: <form>)
    (defclass <code>)
    ;; Variables
    (defmacro def (symbol-or-function <object>))
    (defmacro set (symbol-or-function <object>))
    (defun eq (<object> <object> -> <boolean>))
    ;; Functions
    (defmacro lambda (signature <form> -> <function>))
    (defun funcall (symbol-or-function args key-args))
    (defun apply (symbol-or-function &rest args &all-keys key-args -> <object>))
    ;; Control
    (defmacro if (<boolean> <form> <form> -> <object>))
    (defmacro progn (&body forms -> <object>))
    (defmacro block (<function> -> <object>))
    (defmacro return-from ((<object> label) &optional <object>))
    (defmacro unwind-protect ((<function> protected) (<function> cleanup) -> <object>))
    ;; Macros and reflective tower
    (defmacro defmacro (macro-signature <function>))
    (defmacro eval-when (situations <form>))
    ;; Forms
    (defun at (<list-form> <int> -> <form>))
    (defun slice (<list-form> <int> &optional <int> -> <form>))
    (defmacro quote (<form> -> <form>))
    (defmacro quasiquote (<form> -> <form>))
    (defmacro unquote (<form>))
    (defmacro unquote-splicing (<list-form>))
    ;; Code objects
    (defun encode (<form> <environment> -> <code>))
    (defun decode (<code> -> <form>))
    ;; Objects
    (defmacro make-instance (<class> &all-keys slot-initializers -> <object>))
    (defmacro call-method (<object> <symbol> &rest args &all-keys key-args -> <object>))
    (defun slot-value (<object> <symbol> -> <symbol>))
    (defun set-slot-value (<object> <symbol> <object>))
    (defun class-of (<object> -> <class>))
    ;; Classes
    (defun make-class ((<symbol> &rest superclasses) &rest slot-specs
                       &key superclasses-mutable-p 
                       slot-specs-mutable-p
                       methods-mutable-p
                       -> <class>))
    (defun set-superclasses (<class> &rest superclasses))
    (defun set-slot-specs (<class> &rest slot-specs))
    (defun set-method (<class> <symbol> <function>))
    (defun subclassp (<class> <class> -> <boolean>))
    ;; Conditions
    (defmacro with-handler (&all-keys handler-bindings <form> -> <object>))
    (defun signal (<condition> -> <object>))
    ;; Evaluation
    (defun eval (<form> -> <object>))
    ;; Modules
    (defmacro defpackage (<symbol> <package>))
    (defmacro structure (&rest definitions -> <package>))
    (defmacro signature (&rest declarations -> <package>))
    (defmacro functor (functor-signature <package> -> <package>))
    (defmacro include (&rest package-or-member-names))
    ;; Native
    (defmacro native ((<string> host-language) -> <object>))
  )
)

;;; References:
;;; Variables, functions, terminology, lexical syntax: Common Lisp
;;; Control flow: Goo
;;; Object system and generic functions: Factor (w/out multimethods)
;;; Condition system: Dylan
;;; Hygienic macros, reflective tower, syntax objects: SRFI-72
;;; Expansion process: R6RS Ch. 10, <top-level body>
;;; Quasiquotation: Alan Bawden, "Quasiquotation in Lisp", Appendix B
;;; Modules and functors: OCaml; (C++)
;;; Inline C: Alien Goo
