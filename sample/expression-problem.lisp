;;; Solution to expression problem with F-bounded polymorphism,
;;; following ``The Expression Problem Revisited Four new solutions
;;; using generics'' by Mads Torgersen, Fig. 3, 4, 5.  mjs, 2010-05-15

(defpackage basic (<basic-expr> <basic-lit> <basic-plus> to-str)
  (deftype T <basic-expr T>)
  (defclass <basic-expr T>)
  (defclass <basic-lit T> (<basic-expr T>)
    ((value <num>)))
  (defclass <basic-plus T> (<basic-expr T>)
    ((left T)
     (right T)))
  (defmethod to-str (<basic-lit .value> -> <str>)
    (num-to-str value))
  (defmethod to-str (<basic-plus .left .right> -> <str>)
    (conc (to-str left) "+" (to-str right))))

(defpackage eval (<eval-expr> <eval-lit> <eval-plus> eval)
  (require basic)
  (deftype T <eval-expr T>)
  (defclass <eval-expr T> (<basic-expr T>))
  (defclass <eval-lit T> (<eval-expr T> <basic-lit T>))
  (defclass <eval-plus T> (<eval-expr T> <basic-plus T>))
  (defmethod eval (<eval-lit .value> -> <num>)
    value)
  (defmethod eval (<eval-plus .left .right> -> <num>)
    (+ (eval left) (eval right))))

(defpackage basic-neg (<basic-neg>)
  (require basic)
  (deftype T <basic-expr T>)
  (defclass <basic-neg T> (<basic-expr T>)
    ((expr T)))
  (defmethod to-str (<basic-neg .expr> -> <str>)
    (conc "-" (to-str expr))))

(defpackage eval-neg (<eval-neg>)
  (require basic basic-neg eval)
  (deftype T <eval-expr T>)
  (defclass <eval-neg T> (<eval-expr T> <basic-neg T>))
  (defmethod eval (<eval-neg .expr> -> <num>)
    (- (eval expr))))

(defpackage fix (<expr> <lit> <plus> <neg>)
  (require basic eval basic-neg eval-neg)
  (defclass <expr> (<eval-expr <expr>>))
  (defclass <lit> (<eval-lit <lit>> <expr>)
    (:constructor lit (value)))
  (defclass <plus> (<eval-plus <plus>> <expr>)
    (:constructor plus (left right)))
  (defclass <neg> (<eval-neg <neg>> <expr>)
    (:constructor neg (expr))))

(require basic eval fix)

(defvar *expr* (plus (neg (lit 1)) (lit 4))) ; -1 + 4
(eval *expr*)
(to-str *expr*)
