(provide Collection all
         Range emptyp 
         front pop-front set-front 
         double-ended-p back pop-back set-back
         Concurrent-modification
         List
         Set 
         Dict Dict-entry
         find map)

(deftype A)
(deftype B)

(defclass (Collection A))
(defclass (Range A) (Collection A))
(defgeneric all ((Collection A) -> (Range A)))
(defmethod all ((range (Range A)) -> (Range A)) range)

(defgeneric emptyp ((Range A) -> Boolean))
(defgeneric front ((Range A) -> A))
(defgeneric pop-front ((Range A)))
(defgeneric set-front ((Range A) A -> A))
(defgeneric double-ended-p ((Range A) -> Boolean))
(defgeneric back ((Range A) -> A))
(defgeneric pop-back ((Range A)))
(defgeneric set-back ((Range A) A -> A))

(define-condition Concurrent-modification)

(defclass (List A) ((Collection A))
  ((version <num>)))

(defclass (Dict A B))
(defclass (Dict-entry A B))
(defgeneric get ((Dict A B) A -> B))
(defgeneric contains-key-p ((Dict A B) A -> Boolean))
(defgeneric put ((Dict A B) A B))
(defgeneric remove ((Dict A B) A))
(defgeneric keys ((Dict A B) -> (Collection A)))
(defgeneric values ((Dict A B) -> (Collection B)))
(defgeneric entries ((Dict A B) -> (Collection (Dict-entry A B))))

(defclass (Set A) ((Collection A)))
(defgeneric add ((Set A) A))
(defgeneric containsp ((Set A) A -> Boolean))
(defgeneric remove ((Set A) A))

(defmethod find (collection element &key (test 'equal))
  (proclaim (ftype (Collection A) A &key ((test (A A -> Boolean)) -> (Range A))))
  (let ((range (range collection)))
    (while (not (emptyp range))
      (if (funcall test elt (front range))
          (return)
          (pop-front range)))
    range))

(defmethod map (collection function)
  (proclaim (ftype (Collection A) (A -> B) -> (Collection B)))
  (let ((range (range collection))
        (result (List B)))
    (while (not (emptyp range))
      (add result (funcall function (front range)))
      (pop-front range))
    result))
