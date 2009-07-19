(defpackage Generic-Lisp-Kernel ()
  (signature
    ;;;; Built-in classes and objects
    ;; Symbols can be package scoped, syntax: Package::symbol
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
    ;; + variable access by name
    (defmacro defparameter (<symbol> <object>))
    (defmacro setq (<symbol> <object>))
    (defun boundp (<symbol> -> <boolean>))
    ;;;; Functions
    ;; parameters: required, &optional, &rest, &key, &all-keys, evaluated l-t-r
    ;; lambdas may bind functions with (function var) parameters
    ;; + function or macro call (operator &rest &all-keys)
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
    ;; macros destructure arguments, and have additional &environment
    ;; (for use with FORM->SYNTAX) and &whole parameters.
    (defmacro defmacro (macro-signature <function>))
    (defmacro let-macro (expander-bindings <form> -> <object>))
    ;;;; Forms and syntax objects
    ;; syntax objects are forms with lexical binding information for hygiene,
    ;; they can be used interchangeably with code that requires forms
    ;; Syntax: ' = SYNTAX and ` = QUASISYNTAX, , and ,@
    (defun elt (<list> <int> -> <form>))
    (defun subseq (<list> <int> &optional <int> -> <list>))
    (defmacro syntax (<form> -> <syntax>))
    (defmacro quasisyntax (<form> -> <syntax>))
    (defun form->syntax (<form> <environment> -> <syntax>)) ; for creating syntax in a particular lexical 
    ;;;; Objects                                            ; environment (breaking hygiene)
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
    (defmacro eval-when-compile (<form>)) ; nestable for higher meta-levels
    ;;;; Packages
    ;; + functor application where <package> is allowed: (Functor &rest)
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
