(let ((progn "foo"))
  (when #t
    (send "win" 'print-object)))

(let ((not (lambda (x) x)) (when 'never))
  (unless #f
    (send "win" 'print-object)))

(defmacro sc:or (e1 e2)
  #`(let ((t ,e1))
      (if t t ,e2)))

(send (let ((t "okay"))
        (sc:or #f t))
      'print-object)
