(defpackage Lisp-Kernel ()
  (interface
    ;; Built-in types and objects
    (defclass <object>)
    (defclass <type> <: <object>)
    (defclass <class> <: <type>)
    (defclass <mixin> <: <type>)
    (defclass <null> <: <object>)
    (defvar nil)
    (defclass <boolean> <: <object>)
    (defvar true)
    (defvar false)
    (defclass <form> <: <object>)
    (defclass <symbol> <: <form>)
    (defclass <list> <: <form>)
    ;; Variables
    (defsyntax defparameter (<symbol> <object>))
    (defsyntax setq (<symbol> <object>))
    (defun boundp (<symbol> -> <boolean>))
    ;; Functions
    (defsyntax lambda (<signature> <form> -> <function>))
    (defsyntax defun (<symbol> <function>))
    (defsyntax function (<symbol> -> <function>))
    (defun apply (<function> &rest args &all-keys key-args -> <object>))
    (defun funcall (<function> args key-args))
    (defun fboundp (<symbol> -> <boolean>))
    ;; Control
    (defsyntax if (<boolean> <form> <form> -> <object>))
    (defsyntax progn (&body forms -> <object>))
    (defsyntax unwind-protect (<form> <form> -> <object>))
    (defun call/ec (<function> -> <object>))
    ;; Syntax transformers
    (defsyntax defsyntax (<signature> <function>))
    (defsyntax let-syntax (transformer-bindings <form> -> <object>))
    (defsyntax let*-syntax (transformer-bindings <form> -> <object>))
    (defsyntax eval-for-syntax (<form>))
    ;; Syntax objects
    (defun first (<list> -> <form>))
    (defun rest (<list> -> <form>))
    (defun elt (<list> <number> -> <form>))
    (defun syntax (<form> -> <form>))
    (defun quasisyntax (<form> -> <form>))
    (defun datum->syntax-object (<environment> <form> -> <form>))
    ;; Types and Objects
    (defun set-method (<type> <symbol> <function>))
    (defun call-method (<object> <symbol> &rest args &all-keys key-args -> <object>))
    (defun slot-value (<object> <symbol> -> <symbol>))
    (defun set-slot-value (<object> <symbol> <object>))
    (defun class-of (<object> -> <class>))
    ;; Classes
    (defun make-class (<symbol>
                       &key super-class super-class-mutable-p 
                            mixins mixins-mutable-p 
                            slot-specs-mutable-p
                       &rest slot-specs 
                       -> <class>))
    (defun super-class (<class> -> <class>))
    (defun set-super-class (<class> <class>))
    (defun set-slot-specs (<class> &rest slot-specs))
    ;; Mixins
    (defun make-mixin (<symbol> &key mutable-mixins-p &rest mixins -> <mixin>))
    (defun add-mixin (<type> <mixin>))
    (defun remove-mixin (<type> <mixin>))
    ;; Conditions
    (defsyntax let-handler (handler-binding <form> -> <object>))
    (signal (<condition> -> <object>))
    ;; Evaluation and Compilation
    (defun eval (<form> -> <object>))
    ;; Packages
    (deftype <package>)
    (defsyntax defpackage (<symbol> <signature> <package>))
    (defsyntax implementation (<definitions> -> <package>))
    (defsyntax interface (<declarations> -> <package>))
    (defsyntax use (&all-keys package-bindings))
    ;; UNIX
    (defsyntax c (<string> -> <object>))
  )
)
