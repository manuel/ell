;;; Solution to expression problem with F-bounded polymorphism,
;;; following ``The Expression Problem Revisited Four new solutions
;;; using generics'' by Mads Torgersen, Fig. 3, 4, 5.  mjs, 2010-05-15

(defmodule basic ()
  (signature <basic-expr> <basic-lit> <basic-plus> to-str)
  (structure
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
     (conc (to-str left) "+" (to-str right)))))

(defmodule eval (basic)
  (signature <eval-expr> <eval-lit> <eval-plus> eval)
  (structure
   (open basic)
   (deftype T <eval-expr T>)
   (defclass <eval-expr T> (<basic-expr T>))
   (defclass <eval-lit T> (<eval-expr T> <basic-lit T>))
   (defclass <eval-plus T> (<eval-expr T> <basic-plus T>))
   (defmethod eval (<eval-lit .value> -> <num>)
     value)
   (defmethod eval (<eval-plus .left .right> -> <num>)
     (+ (eval left) (eval right)))))

(defmodule basic-neg (basic)
  (signature <basic-neg>)
  (structure
   (open basic)
   (deftype T <basic-expr T>)
   (defclass <basic-neg T> (<basic-expr T>)
     ((expr T)))
   (defmethod to-str (<basic-neg .expr> -> <str>)
     (conc "-" (to-str expr)))))

(defmodule eval-neg (basic basic-neg eval)
  (signature <eval-neg>)
  (structure
   (open basic basic-neg eval)
   (deftype T <eval-expr T>)
   (defclass <eval-neg T> (<eval-expr T> <basic-neg T>))
   (defmethod eval (<eval-neg .expr> -> <num>)
     (- (eval expr)))))

(defmodule fix (basic eval basic-neg eval-neg)
  (signature <expr> <lit> <plus> <neg>)
  (structure
   (open basic eval basic-neg eval-neg)
   (defclass <expr> (<eval-expr <expr>>))
   (defclass <lit> (<eval-lit <lit>> <expr>)
     (:constructor lit (value)))
   (defclass <plus> (<eval-plus <plus>> <expr>)
     (:constructor plus (left right)))
   (defclass <neg> (<eval-neg <neg>> <expr>)
     (:constructor neg (expr)))))

(open basic eval fix)

(defvar *expr* (plus (neg (lit 1)) (lit 4))) ; -1 + 4
(eval *expr*)
(to-str *expr*)
