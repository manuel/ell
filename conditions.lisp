(defclass <handler>)

(defclass <default-handler> (<handler>))

(defclass <user-handler> (<handler>)
  condition-class
  handler-function
  next-handler)

(defmethod print-object ((h <handler>))
  (print-object '#<handler>))

(defun make-user-handler (condition-class handler-function)
  (let ((h (make <user-handler>)))
    (set-slot-value h 'condition-class condition-class)
    (set-slot-value h 'handler-function handler-function)
    (set-slot-value h 'next-handler *current-handler*)
    h))

(defparameter *current-handler* (make <default-handler>))

(defgeneric handle-condition (handler condition))

(defmethod handle-condition ((h <default-handler>) condition)
  (print condition)
  (stacktrace)
  (exit))

(defmethod handle-condition ((h <user-handler>) condition)
  (if (handler-matches? h condition)
      (funcall (slot-value h 'handler-function)
               condition
               (lambda ()
                 (handle-condition (slot-value h 'next-handler) condition)))
      (handle-condition (slot-value h 'next-handler) condition)))

(defgeneric handler-matches? (user-handler condition))

(defmethod handler-matches? ((h <user-handler>) condition)
  (type? condition (slot-value h 'condition-class)))

(defun signal (condition)
  (handle-condition *current-handler* condition))

(defun with-monitor (condition-class user-handler-function body-thunk)
  (let ((the-handler-function (lambda (condition call-next-handler)
                                (block resume
                                  (funcall user-handler-function
                                           condition
                                           (lambda (value) (return-from resume value)))
                                  (funcall call-next-handler)))))
    (fluid-let *current-handler* (make-user-handler condition-class the-handler-function)
      (funcall body-thunk))))
