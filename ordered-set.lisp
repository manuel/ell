(defpackage Ordered-type
  (interface ()
    (deftype <t>)
    (defun compare (<t> <t> -> <int>))))

(defpackage Set
  (interface ()
    (deftype <t>)
    (deftype <elt>)
    (defun make (-> <t>))
    (defun add (<t> <elt>))))

(defpackage Set-impl
  (implementation ((Elt Ordered-type))
    (defclass <t> (elements <list> init: (list)))
    (deftype <elt> Elt)
    (defun make (-> <t>) (make-t))
    (defun add (<t> <elt>) (add (.elements t) elt)))) ; blah

(defpackage Abstract-set 
  (implementation ((Elt Ordered-type) -> (Set <elt> = Elt::<t>))
    Set-impl))

;; --

(defpackage No-case-string
  (implementation ()
    (deftype <t> <str>)
    (defun compare ((s1 <str>) (s2 <str>) -> <int>)
      (strcmp (tolower s1) (tolower s2)))))

(defpackage No-case-string-set
  (Abstract-set No-case-string))

;; --

(defpackage My
  (implementation ()
    (use-package S = No-case-string-set)
    (let set = (S::make)
      (S::add set "foo"))))

;; Now I just need to figure out how to compile this.

;; http://caml.inria.fr/pub/docs/manual-ocaml/manual004.html
