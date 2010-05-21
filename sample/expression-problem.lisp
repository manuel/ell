;;; Solution to expression problem with F-bounded polymorphism,
;;; following ``The Expression Problem Revisited Four new solutions
;;; using generics'' by Mads Torgersen, Fig. 3, 4, 5.  mjs, 2010-05-15

(defpackage Basic (Basic-expr Basic-lit Basic-plus to-str)
  (deftype T (Basic-expr T))
  (defclass (Basic-expr T))
  (defclass (Basic-lit T) ((Basic-expr T))
    ((value Number)))
  (defclass (Basic-plus T) ((Basic-expr T))
    ((left T)
     (right T)))
  (defmethod to-str ((Basic-lit .value) -> String)
    (num-to-str value))
  (defmethod to-str ((Basic-plus .left .right) -> String)
    (conc (to-str left) "+" (to-str right))))

(defpackage Eval (Eval-expr Eval-lit Eval-plus eval)
  (require Basic)
  (deftype T (Eval-expr T))
  (defclass (Eval-expr T) ((Basic-expr T)))
  (defclass (Eval-lit T) ((Eval-expr T) (Basic-lit T)))
  (defclass (Eval-plus T) ((Eval-expr T) (Basic-plus T)))
  (defmethod eval ((Eval-lit .value) -> Number)
    value)
  (defmethod eval ((Eval-plus .left .right) -> Number)
    (+ (eval left) (eval right))))

(defpackage Basic-neg (Basic-neg)
  (require Basic)
  (deftype T (Basic-expr T))
  (defclass (Basic-neg T) ((Basic-expr T))
    ((expr T)))
  (defmethod to-str ((Basic-neg .expr) -> String)
    (conc "-" (to-str expr))))

(defpackage Eval-neg (Eval-neg)
  (require Basic Basic-neg eval)
  (deftype T (Eval-expr T))
  (defclass (Eval-neg T) ((Eval-expr T) (Basic-neg T)))
  (defmethod eval ((Eval-neg .expr) -> Number)
    (- (eval expr))))

(defpackage fix (Expr Lit Plus Neg)
  (require Basic Eval Basic-neg Eval-neg)
  (defclass Expr ((eval-expr Expr)))
  (defclass Lit ((eval-lit Lit) Expr)
    (:constructor Lit (value)))
  (defclass Plus ((eval-plus Plus) Expr)
    (:constructor Plus (left right)))
  (defclass Neg ((Eval-neg Neg) Expr)
    (:constructor Neg (expr))))

(require Basic Eval Fix)

(defvar *expr* (Plus (Neg (Lit 1)) (Lit 4))) ; -1 + 4
(eval *expr*)
(to-str *expr*)
