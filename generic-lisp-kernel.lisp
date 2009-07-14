(defpackage Generic-Lisp-Kernel ()
  (interface
    ;; Built-in classes and objects
    (defclass <object>)
    (defclass <class>)
    (defclass <function>)
    (defclass <generic> <: <function>)
    (defclass <method> <: <function>)
    (defclass <package>)
    (defclass <form>)
    (defclass <symbol> <: <form>)
    (defclass <list> <: <form>)
    (defclass <null>)
    (defclass <boolean>)
    (defvar nil)
    (defvar true)
    (defvar false)
    ;; Variables
    (defmacro defparameter (<symbol> <object>))
    (defmacro setq (<symbol> <object>))
    (defun boundp (<symbol> -> <boolean>))
    ;; Functions
    (defmacro lambda (<signature> <form> -> <function>))
    (defmacro defun (<symbol> <function>))
    (defmacro function (<symbol> -> <function>))
    (defun apply (<function> &rest args &all-keys key-args -> <object>))
    (defun funcall (<function> args key-args))
    (defun fboundp (<symbol> -> <boolean>))
    ;; Control
    (defmacro if (<boolean> <form> <form> -> <object>))
    (defmacro progn (&body forms -> <object>))
    (defmacro unwind-protect (<form> <form> -> <object>))
    (defun call-with-escape-continuation (<function> -> <object>))
    ;; Syntax transformers and reflective tower
    (defmacro defmacro (<signature> <function>))
    (defmacro let-macro (transformer-bindings <form> -> <object>))
    ;; Syntax objects
    (defun first (<list> -> <form>))
    (defun rest (<list> -> <list>))
    (defun elt (<list> <number> -> <form>))
    (defun quote (<form> -> <form>))
    (defun quasiquote (<form> -> <form>))
    (defun datum->syntax-object (<environment> <form> -> <form>))
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
    (defmacro defpackage (<symbol> <signature> <package>))
    (defmacro use (&all-keys package-bindings))
    (defmacro interface (&rest declarations -> <package>))
    (defmacro implementation (&rest definitions -> <package>))
    (defmacro include (&rest packages))
    ;; UNIX
    (defmacro c (<string> -> <object>))
  )
)

;; Control flow and condition system: Dylan, Goo, Common Lisp, PLT Scheme
;; Inline C: Alien Goo
;; Packages and functors: OCaml, C++, Dylan, PLOT
;; Object system and generic functions: CLOS, Dylan, Factor, C++
;; Hygienic macros, reflective tower, syntax objects: SRFI-72
;; Variables, functions, terminology: Common Lisp, Dylan, Goo, PLOT
;; Expansion process: R6RS Ch. 10
