;;; Solution to expression problem with F-bounded polymorphism,
;;; following ``The Expression Problem Revisited Four new solutions
;;; using generics'' by Mads Torgersen, Fig. 3, 4, 5.  mjs, 2010-05-15

(defpackage Basic (Basic-Expr Basic-Lit Basic-Plus to-str)
  (deftype T (Basic-Expr T))
  (defclass (Basic-Expr T))
  (defclass (Basic-Lit T) ((Basic-Expr T))
    ((value Number)))
  (defclass (Basic-Plus T) ((Basic-Expr T))
    ((left T)
     (right T)))
  (defmethod to-str ((Basic-Lit .value) -> String)
    (num-to-str value))
  (defmethod to-str ((Basic-Plus .left .right) -> String)
    (conc (to-str left) "+" (to-str right))))

(defpackage Eval (Eval-Expr Eval-Lit Eval-Plus eval)
  (require Basic)
  (deftype T (Eval-Expr T))
  (defclass (Eval-Expr T) ((Basic-Expr T)))
  (defclass (Eval-Lit T) ((Eval-Expr T) (Basic-Lit T)))
  (defclass (Eval-Plus T) ((Eval-Expr T) (Basic-Plus T)))
  (defmethod eval ((Eval-Lit .value) -> Number) value)
  (defmethod eval ((Eval-Plus .left .right) -> Number) (+ (eval left) (eval right))))

(defpackage Basic-neg (Basic-neg)
  (require Basic)
  (deftype T (Basic-Expr T))
  (defclass (Basic-Neg T) ((Basic-Expr T)) 
    ((expr T)))
  (defmethod to-str ((Basic-Neg .expr) -> String) (conc "-" (to-str expr))))

(defpackage Eval-Neg (Eval-Neg)
  (require Basic Basic-Neg Eval)
  (deftype T (Eval-Expr T))
  (defclass (Eval-Neg T) ((Eval-Expr T) (Basic-Neg T)))
  (defmethod eval ((Eval-Neg .expr) -> Number) (- (eval expr))))

(defpackage Fix (Expr Lit Plus Neg)
  (require Basic Eval Basic-Neg Eval-Neg)
  (defclass Expr ((Eval-Expr Expr)))
  (defclass Lit ((Eval-Lit Lit) Expr) (:constructor Lit (value)))
  (defclass Plus ((Eval-Plus Plus) Expr) (:constructor Plus (left right)))
  (defclass Neg ((Eval-Neg Neg) Expr) (:constructor Neg (expr))))

(require Basic Eval Fix)

(defvar *expr* (Plus (Neg (Lit 1)) (Lit 4))) ; -1 + 4
(eval *expr*)
(to-str *expr*)
