(defclass <condition>)
(defclass <unbound-variable> (<condition>)
  name)
(defclass <unbound-function> (<condition>)
  name)

(defclass <restart> (<condition>))
(defclass <use-value> (<restart>)
  value)

(defclass <handler>)

(defclass <default-handler> (<handler>))

(defclass <user-handler> (<handler>)
  condition-class
  handler-function
  next-handler)

(defmethod print-object ((h <handler>))
  (print-object '#<handler>))

(defun make-user-handler (condition-class handler-function next-handler)
  (let ((h (make <user-handler>)))
    (set-slot-value h 'condition-class condition-class)
    (set-slot-value h 'handler-function handler-function)
    (set-slot-value h 'next-handler next-handler)
    h))

(defparameter *current-handler* (make <default-handler>))

(defgeneric handle-condition (handler condition))

(defmethod handle-condition ((h <default-handler>) (c <condition>))
  (print condition)
  (stacktrace)
  (exit))

(defmethod handle-condition ((h <user-handler>) (c <condition>))
  (if (handler-matches? h condition)
      (funcall (slot-value h 'handler-function)
               condition
               (lambda ()
                 (handle-condition (slot-value h 'next-handler) condition)))
      (handle-condition (slot-value h 'next-handler) condition)))

(defgeneric handler-matches? (user-handler condition))

(defmethod handler-matches? ((h <user-handler>) (c <condition>))
  (type? condition (slot-value h 'condition-class)))

(defun handler-bind/f (condition-class user-handler-function body-thunk)
  (let ((the-handler-function (lambda (condition call-next-handler)
                                (block resume
                                  (funcall user-handler-function
                                           condition
                                           (lambda (value) (return-from resume value)))
                                  (funcall call-next-handler)))))
    (fluid-let *current-handler* (make-user-handler condition-class
                                                    the-handler-function
                                                    *current-handler*)
      (funcall body-thunk))))

(defmacro handler-bind (condition-class user-handler-function &rest body)
  #`(handler-bind/f ,condition-class ,user-handler-function
                    (lambda () ,@body)))

(defun signal (condition)
  (handle-condition *current-handler* condition))

(defun warn (condition)
  (signal condition)
  (print condition))

(defun error (condition)
  (signal condition)
  (invoke-debugger condition))

(defun cerror (condition)
  (block use-value
    (handler-bind <use-value> (lambda (restart resume)
                                (return-from (slot-value restart 'value)))
      (error condition))))

