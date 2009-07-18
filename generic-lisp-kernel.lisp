(defpackage Generic-Lisp-Kernel ()
  (signature
    ;; Built-in classes and objects
    (defclass <package>)
    (defclass <class>)
    (defclass <object>)
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
    (defclass <ast>)
    ;; Variables
    (defmacro defparameter (<symbol> <object>))
    (defmacro setq (<symbol> <object>))
    (defun boundp (<symbol> -> <boolean>))
    ;; Functions
    (defmacro lambda (signature <form> -> <function>))
    (defmacro defun (<symbol> <function>))
    (defmacro function (<symbol> -> <function>))
    (defun apply (<function> &rest args &all-keys key-args -> <object>))
    (defun funcall (<function> args key-args))
    (defun fboundp (<symbol> -> <boolean>))
    ;; Control
    (defmacro if (<boolean> <form> <form> -> <object>))
    (defmacro progn (&body forms -> <object>))
    (defmacro block ((<symbol> label-name) <form> -> <object>))
    (defmacro return-from ((<object> label) <object>))
    (defmacro unwind-protect ((<form> protected) (<form> cleanup) -> <object>))
    ;; Macros
    (defmacro defmacro (macro-signature <function>))
    (defmacro let-macro (expander-bindings <form> -> <object>))
    ;; Forms and abstract syntax trees
    (defun elt (<list> <int> -> <form>))
    (defun subseq (<list> <int> &optional <int> -> <list>))
    (defun quote (<form> -> <ast>))
    (defun quasiquote (<form> -> <ast>))
    (defun form->ast (<form> <environment> -> <ast>))
    ;; Objects
    (defun call-method (<object> <symbol> &rest args &all-keys key-args -> <object>))
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
    (defmacro let-handler (handler-bindings <form> -> <object>))
    (defun signal (<condition> -> <object>))
    ;; Evaluation and reflective tower
    (defun eval (<form> -> <object>))
    (defmacro eval-when-compile (<form>))
    ;; Packages
    (defmacro defpackage (<symbol> <package>))
    (defmacro signature (&rest declarations -> <package>))
    (defmacro structure (&rest definitions -> <package>))
    (defmacro functor (functor-signature <package> -> <package>))
    (defmacro use (&all-keys package-bindings))
    (defmacro include (&rest package-names))
    (defmacro include-variable (&rest fqns))
    (defmacro include-function (&rest fqns))
    (defmacro include-macro (&rest fqns))
    ; + functor application where <package> is allowed: (Functor &rest args)
    ;; Native
    (defmacro native (<string> -> <object>))
  )
)

;; Control flow and condition system: Dylan, Goo, Common Lisp, PLT Scheme
;; Inline C: Alien Goo
;; Packages and functors: OCaml, C++, Dylan, PLOT
;; Object system and generic functions: CLOS, Dylan, Factor, C++
;; Hygienic macros, reflective tower, syntax objects: SRFI-72
;; Variables, functions, terminology: Common Lisp, Dylan, Goo, PLOT
;; Expansion process: R6RS Ch. 10
