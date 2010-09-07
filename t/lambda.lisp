"-- booleans --"

(defun db (b) (funcall b "true" "false"))

(defvar true (lambda (tt ff) tt))
(defvar false (lambda (tt ff) ff))

(defun either (p c a) (funcall p c a))

"-- pairs --"

(defun pair (x y) (lambda (s) (funcall s x y)))
(defun fst (p) (funcall p true))
(defun snd (p) (funcall p false))

"-- numerals --"

(defun dn (n) (funcall n 0 (lambda (c) (+ c 1))))

(defvar zero (lambda (z s) z))
(defvar one (lambda (z s) (funcall s z)))
(defvar two (lambda (z s) (funcall s (funcall s z))))

(defun zero? (n) (funcall n true (lambda (z) false)))

(defun add (n m) (lambda (z s) (funcall m (funcall n z s) s)))
(defun mul (n m) (funcall m zero (lambda (c) (add n c))))

(defun dec (n) (fst (funcall n (pair zero zero) (lambda (p) (pair (snd p) (add (snd p) one))))))

"-- factorial --"

(defun fact (n) (funcall (either (zero? n) (lambda () one) (lambda () (mul n (fact (dec n)))))))

"-- tests --"

(db true)
(db false)
(db (either true true false))
(db (either false true false))

(dn zero)
(dn one)
(db (zero? zero))
(db (zero? one))

(dn (add two two))
(dn (mul two two))

(dn (dec two))

(dn (fact zero))
(dn (fact one))
(dn (fact two))
(dn (fact (add two one)))
(dn (fact (add two two)))
