;;; Solution to expression problem with f-bounded polymorphism,
;;; following ``The Expression Problem Revisited Four new solutions
;;; using generics'' by Mads Torgersen, Fig. 3, 4, 5.  mjs, 2010-05-15

(defpackage basic (<basic-expr> <basic-lit> <basic-plus> to-str)
  (defclass (<basic-expr> (<: C (<basic-expr> C))))
  (defclass (<basic-lit> (<: C (<basic-expr> C))) ((<basic-expr> C))
    ((value <num>)))
  (defclass (<basic-plus> (<: C (<basic-expr> C))) ((<basic-expr> C))
    ((left C)
     (right C)))
  (defmethod to-str (<lit .value> -> <str>)
    (num-to-str value))
  (defmethod to-str (<plus .left .right> -> <str>)
    (conc (to-str left) "+" (to-str right))))

(defpackage eval (<eval-expr> <eval-lit> <eval-plus> eval)
  (require basic)
  (defclass (<eval-expr> (<: C (<eval-expr> C))) ((<basic-expr> C)))
  (defclass (<eval-lit> (<: C (<eval-expr> C))) ((<basic-lit> C) (<eval-expr> C)))
  (defclass (<eval-plus> (<: C (<eval-expr> C))) ((<basic-plus> C) (<eval-expr> C)))
  (defmethod eval (<eval-lit .value> -> <num>)
    value)
  (defmethod eval (<eval-plus .left .right> -> <num>)
    (+ (eval left) (eval right))))

(defpackage basic-neg (<basic-neg>)
  (require basic)
  (defclass (<basic-neg> (<: C (<basic-expr> C))) ((<basic-expr> C))
    ((expr C)))
  (defmethod to-str (<neg .expr> -> <str>)
    (conc "-" (to-str expr))))

(defpackage eval-neg (<eval-neg>)
  (require basic basic-neg eval)
  (defclass (<eval-neg> (<: C (<eval-expr> C))) ((<basic-neg> C) (<eval-expr> C)))
  (defmethod eval (<eval-neg .expr> -> <num>)
    (- (eval expr))))

(defpackage fix (<expr> <lit> <plus> <neg>)
  (require basic eval basic-neg eval-neg)
  (defclass <expr> ((<eval-expr> <expr>)))
  (defclass <lit> ((<eval-lit> <lit>) <expr>)
    (:constructor lit (value)))
  (defclass <plus> ((<eval-plus> <plus>) <expr>)
    (:constructor plus (left right)))
  (defclass <neg> ((<eval-neg> <neg>) <expr>)
    (:constructor neg (expr))))

(require basic eval fix)

(defvar *expr* (plus (neg (lit 1)) (lit 4))) ; -1 + 4
(eval *expr*)
(to-str *expr*)
