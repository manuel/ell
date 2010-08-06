(defsyntax if-it
  (lambda (form)
    (apply-syntax-list (lambda (k e1 e2 e3)
                         (let ((it (datum->syntax k 'it)))
                           #`(let ((,it ,e1))
                               (if ,it ,e2 ,e3))))
                       form)))

(defsyntax when-it
  (lambda (form)
    (apply-syntax-list (lambda (k e1 e2)
                         (let ((the-it (datum->syntax k 'it)))
                           #`(if-it ,e1
                               (let ((,the-it it)) ,e2)
                               (if #f #f))))
                       form)))

(defmacro my-or (e1 e2)
  #`(if-it ,e1 it ,e2))

(print-object (if-it "2" it "3"))
(print-object (when-it "42" it))
(print-object (my-or "2" "3"))
  
(print-object (let ((it "1")) (if-it "42" it #f)))
(print-object (let ((it "1")) (when-it "42" it)))
(print-object (let ((it "1")) (my-or "42" it)))
(print-object (let ((it "1")) (my-or #f it)))
(print-object (let ((if-it "1")) (when-it "42" it)))
