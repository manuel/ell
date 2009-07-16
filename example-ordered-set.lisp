;;; The classical set example of higher-order modules from
;;; http://caml.inria.fr/pub/docs/manual-ocaml/manual004.html 
;;;
;;; Also shows how functors are instantiated and how finally modules
;;; are erased from the resulting code by the compiler.

(defpackage Ordered-Type
  (signature
    (defclass <t>)
    (defun compare (<t> <t> -> <int>))))

(defpackage Set
  (signature
    (defclass <t>)
    (defclass <elt>)
    (defun make (-> <t>))
    (defun add (<t> <elt>))))

(defpackage Set-Impl ((Ordered-Type Elt))
  (structure
    (defclass <t> (<list> elements init: (list)))
    (deftype <elt> Elt::<t>)
    (defun make (-> <t>) (make-t))
    (defun add (<t> <elt>) (... (Elt::compare elt ...) ...))))

(defpackage Make-Set ((Ordered-Type Elt) -> Set)
  (Set-Impl Elt))

;;;

(defpackage CI-String
  (structure
    (deftype <t> <str>)
    (defun compare ((<str> s1) (<str> s2) -> <int>)
      (declare (inline))
      (strcmp (tolower s1) (tolower s2)))))

(defpackage CI-String-Set
  (Make-Set CI-String))

;;;

(defpackage My
  (structure
    (use S = CI-String-Set)
    (let set = (S::make)
      (S::add set "foo"))))

;; after defunctorization:

(defpackage My
  (structure
    (let set = (Set-Impl##CI-String::make)
      (Set-Impl##CI-String::add set "foo"))))

(defpackage Set-Impl##CI-String
  (structure
    (defclass <t> (<list> elements init: (list)))
    (deftype <elt> CI-String::<t>)
    (defun make (-> <t>) (make-t))
    (defun add (<t> <elt>) (... (strcmp (tolower elt) (tolower ...)) ...)))) ; inlining

;; after demodularization:

(let My::set = (Set-Impl##CI-String::make)
  (Set-Impl##CI-String::add My::set "foo"))

(defclass Set-Impl##CI-String::<t> (<list> elements init: (list)))
(deftype Set-Impl##CI-String::<elt> CI-String::<t>)
(defun Set-Impl##CI-String::make (-> Set-Impl##CI-String::<t>) (Set-Impl##CI-String::make-t))
(defun Set-Impl##CI-String::add (Set-Impl##CI-String::<t> Set-Impl##CI-String::<elt>)
  (... (strcmp (tolower elt) (tolower ...)) ...))
