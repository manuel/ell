(defpackage Ordered-Type ()
  (interface
    (deftype <t>)
    (defun compare (<t> <t> -> <int>))))

(defpackage Set ()
  (interface
    (deftype <t>)
    (deftype <elt>)
    (defun make (-> <t>))
    (defun add (<t> <elt>))))

(defpackage Set-Impl ((Elt Ordered-Type))
  (implementation
    (defclass <t> (elements <list> init: (list)))
    (deftype <elt> Elt)
    (defun make (-> <t>) (make-t))
    (defun add (<t> <elt>) (add (.elements t) elt)))) ; blah

(defpackage Abstract-Set ((Elt Ordered-Type) -> (Set <elt> = Elt::<t>))
  Set-Impl)

;; --

(defpackage CI-String ()
  (implementation
    (deftype <t> <str>)
    (defun compare ((s1 <str>) (s2 <str>) -> <int>)
      (strcmp (tolower s1) (tolower s2)))))

(defpackage CI-String-Set ()
  (Abstract-Set CI-String))

;; --

(defpackage My ()
  (implementation
    (use S = CI-String-Set)
    (let set = (S::make)
      (S::add set "foo"))))

;; Now I just need to figure out how to compile this.

;; http://caml.inria.fr/pub/docs/manual-ocaml/manual004.html
