(defpackage Lisp-kernel
  (interface ()
    ;; Built-in types and objects
    (defclass <object>)
    (defclass <type> <: <object>)
    (defclass <class> <: <type>)
    (defclass <mixin> <: <type>)
    (defclass <form> <: <object>)
    (defclass <symbol> <: <form>)
    (defclass <list> <: <form>)
    (defclass <boolean> <: <object>)
    (defclass <true> <: <boolean>)
    (defclass <false> <: <boolean>)
    (defvar true)
    (defvar false)
    (defclass <null> <: <object>)
    (defvar nil)
    ;; Variables
    (defsyntax defparameter (<symbol> <object>))
    (defsyntax setq (<symbol> <object>))
    (defun boundp (<symbol> -> <boolean>))
    ;; Functions
    (defsyntax defun (<symbol> <function>))
    (defsyntax function (<symbol> -> <function>))
    (defsyntax lambda (<signature> <form> -> <function>))
    (defun apply (<function> &rest args &all-keys key-args -> <object>))
    (defun fboundp (<symbol> -> <boolean>))
    (defun funcall (<function> args key-args))
    ;; Control
    (defsyntax if (<boolean> <form> <form> -> <object>))
    (defsyntax progn (&body forms -> <object>))
    (defsyntax unwind-protect (<form> <form> -> <object>))
    (defun call/ec (<function> -> <object>))
    ;; Syntax transformers
    (defsyntax defsyntax (<signature> <function>))
    (defsyntax let-syntax (transformer-bindings <form> -> <object>))
    (defsyntax let*-syntax (transformer-bindings <form> -> <object>))
    ;; Syntax objects
    (defun datum->syntax-object (<environment> <form> -> <form>))
    (defun elt (<list> <number> -> <form>))
    (defun first (<list> -> <form>))
    (defun quasisyntax (<form> -> <form>))
    (defun rest (<list> -> <form>))
    (defun syntax (<form> -> <form>))
    ;; Classes
    (defun make-class (<symbol>
                       &key super-class mutable-super-class-p mutable-super-mixins-p mutable-slot-specs-p
                       &rest slot-specs 
                       -> <class>)
    (defun set-method (<class> <symbol> <function>))
    (defun set-slot-specs (<class> &rest slot-specs))
    (defun set-super-class (<class> <class>))
    ;; Mixins
    (defun add-mixin (<type> <mixin>))
    (defun make-mixin (<symbol> &key mutable-super-mixins-p &rest super-mixins -> <mixin>))
    (defun remove-mixin (<type> <mixin>))
    ;; Objects
    (defun call-applicable-method (<object> <symbol> &rest args &all-keys key-args -> <object>))
    (defun set-slot-value (<object> <symbol> <object>))
    (defun slot-value (<object> <symbol> -> <symbol>))
    (defun type-of (<object> -> <type>))
    ;; Conditions
    (defsyntax let-handler (handler-binding <form> -> <object>))
    (signal (<condition> -> <object>))
    ;; Evaluation and Compilation
    (defsyntax define-compiler-macro (<symbol> <function>))
    (defun eval (<form> -> <object>))
    (defsyntax eval-when-compile (<form>))
    ;; Packages
    (deftype <package>)
    (defsyntax defpackage (<symbol> <package-or-functor-application>))
    (defsyntax implementation (<signature> <definitions> -> <package>))
    (defsyntax interface (<signature> <declarations> -> <package>))
    (defsyntax use (&key package))
    ;; UNIX
    (defsyntax c (<string> -> <object>))
  )
)
