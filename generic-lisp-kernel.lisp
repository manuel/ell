(defpackage Generic-Lisp-Kernel
  (signature
    ;;;; Built-in classes and objects
    (defclass <object>)
    (defclass <class>)
    (defclass <package>)
    (defclass <function>)
    (defclass <generic> <: <function>)
    (defclass <method> <: <function>)
    (defclass <condition>)
    (defclass <restart> <: <condition>)
    (defclass <boolean>)
    (defclass <null>)
    (defvar true)
    (defvar false)
    (defvar nil)
    (defclass <form>)
    (defclass <symbol> <: <form>)
    (defclass <list> <: <form>)
    (defclass <syntax> <: <form>)
    (defclass <symbol-syntax> <: (<syntax> <symbol>))
    (defclass <list-syntax> <: (<syntax> <list>))
    ;;;; Variables
    (defmacro defparameter (<symbol> <object>))
    (defmacro setq (<symbol> <object>))
    (defun boundp (<symbol> -> <boolean>))
    ;;;; Functions
    (defmacro lambda (signature <form> -> <function>))
    (defmacro defun (<symbol> <function>))
    (defmacro function (<symbol> -> <function>))
    (defun apply (function-or-symbol &rest args &all-keys key-args -> <object>))
    (defun funcall (function-or-symbol args key-args))
    (defun fboundp (<symbol> -> <boolean>))
    ;;;; Control
    (defmacro if (<boolean> <form> <form> -> <object>))
    (defmacro progn (&body forms -> <object>))
    (defmacro block ((<symbol> label-name) <form> -> <object>)) ; first-class label
    (defmacro return-from ((<object> label) <object>))
    (defmacro unwind-protect ((<form> protected) (<form> cleanup) -> <object>))
    ;;;; Macros
    (defmacro defmacro (macro-signature <function>))
    (defmacro let-macro (expander-bindings <form> -> <object>))
    ;;;; Forms and syntax objects
    (defun elt (<list> <int> -> <form>))
    (defun subseq (<list> <int> &optional <int> -> <list>))
    (defmacro syntax (<form> -> <syntax>))
    (defmacro quasisyntax (<form> -> <syntax>))
    (defun form->syntax (<form> <environment> -> <syntax>))
    ;;;; Objects
    (defun call-method (<object> <symbol> &rest args &all-keys key-args -> <object>))
    (defun slot-value (<object> <symbol> -> <symbol>))
    (defun set-slot-value (<object> <symbol> <object>))
    (defun class-of (<object> -> <class>))
    ;;;; Classes
    (defun make-class ((<symbol> &rest superclasses) &rest slot-specs
                       &key superclasses-mutable-p 
                       slot-specs-mutable-p
                       methods-mutable-p
                       -> <class>))
    (defun set-superclasses (<class> &rest superclasses))
    (defun set-slot-specs (<class> &rest slot-specs))
    (defun set-method (<class> <symbol> <function>))
    (defun subclassp (<class> <class> -> <boolean>))
    ;;;; Conditions
    (defmacro let-handler (&all-keys handler-bindings <form> -> <object>))
    (defun signal (<condition> -> <object>))
    ;;;; Evaluation and reflective tower
    (defun eval (<form> -> <object>))
    (defmacro eval-when-compile (<form>))
    ;;;; Packages
    (defmacro defpackage (<symbol> <package>))
    (defmacro signature (&rest declarations -> <package>))
    (defmacro structure (&rest definitions -> <package>))
    (defmacro functor (functor-signature <package> -> <package>))
    (defmacro let-package (&all-keys package-bindings <form>) -> <object>)
    (defmacro include (&rest package-names))
    (defmacro include-variable (&rest fqns))
    (defmacro include-function (&rest fqns))
    (defmacro include-macro (&rest fqns))
    ;;;; Native
    (defmacro native ((<string> host-language) -> <object>))
  )
)

;;; This is the low-level form into which all code is translated by
;;; the compiler.  In a sense, it can be viewed as the instruction set
;;; of a Lisp (virtual) machine.
;;;
;;; As usual, a form can be a literal (evaluates to itself), a symbol
;;; (evaluates to the value of the symbol's binding), or a list
;;; (evaluates to a function call, or is a macro that expands to
;;; another form, or is a special form).
;;;
;;; Symbols can be package-scoped, with the syntax `Package::symbol'.
;;;
;;; Functions can have required, `&optional', `&rest', `&key', and
;;; `&all-keys' parameters.  `&all-keys' is populated with a
;;; dictionary of all keyword arguments passed to the function.
;;;
;;; Macros destructure their arguments, and take additional
;;; `&environment' and `&whole' parameters.  `&environment' can be
;;; used with `form->syntax' to break hygiene by inserting code into a
;;; particular lexical scope.
;;;
;;; Package expressions are written in a lazy functional language, so
;;; functor applications of the form (Functor &rest) are allowed
;;; wherever a `<package>' object is allowed.
;;;
;;; Code can be lifted into a higher meta-level with
;;; `eval-when-compile'.  By nesting, code can be inserted into
;;; arbitrary meta-levels.
;;;
;;; References:
;;; Control flow and condition system: Dylan
;;; Inline C: Alien Goo
;;; Packages and functors: OCaml
;;; Object system and generic functions: Factor (w/out multimethods)
;;; Hygienic macros, reflective tower, syntax objects: SRFI-72
;;; Variables, functions, terminology, lexical syntax: Common Lisp
;;; Expansion process: R6RS Ch. 10
;;; Quasiquotation: Alan Bawden, "Quasiquotation in Lisp", Appendix B
