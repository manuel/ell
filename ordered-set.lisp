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
  (implementation ((Elt Ordered-type) -> Set)
    (defclass <t> (elements <list> init: (list)))
    (deftype <elt> Elt)
    (defun make (-> <t>) (make-t))
    (defun add (<t> <elt>) (add (.elements t) elt)))) ; blah

;; --

(defpackage No-case-string
  (implementation (-> Ordered-type)
    (deftype <t> <str>)
    (defun compare ((s1 <str>) (s2 <str>) -> <int>)
      (strcmp (tolower s1) (tolower s2)))))

(defpackage No-case-string-set 
  (Set-impl No-case-string))

;; --

(defpackage My
  (implementation ()
    (let-package S = No-case-string-set
      (let set = (S::make)
        (S::add set "foo")))))

;; Now I just need to figure out how to compile this.
