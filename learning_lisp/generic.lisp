
(defclass bank-account ()
  ((balance :accessor balance :initarg :balance)))

(defclass checking-account (bank-account)
  ((overdraft-account :accessor overdraft-account :initarg :overdraft-account)))

(defclass savings-account (bank-account)
  ())


(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn!"))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))
    (call-next-method)))

(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))


(defparameter *account* (make-instance 'bank-account :balance 100))

(defparameter *backup*
  (make-instance 'checking-account
                 :overdraft-account *account*
                 :balance 120))

(defclass a ()
  ((value :accessor value :initarg :value)))

(defclass b (a)
  ())

(defclass c (b)
  ())

(find-class 'a)


(defgeneric test (object))

(defmethod test ((object a))
  (format t "A~%"))

(defmethod test ((object b))
  (format t "B~%"))

(defmethod test ((object c))
  (format t "C~%"))

(defgeneric test-2 (object))

(defmethod test-2 ((object a))
  (format t "Primary A~%"))

(defmethod test-2 :before ((object a))
  (format t "Before A~%"))

(defmethod test-2 :after ((object a))
  (format t "After A~%"))

(defmethod test-2 :before ((object b))
  (format t "Before B~%"))

(defmethod test-2 :before ((object c))
  (format t "Before C~%"))

(defmethod test-2 :after ((object b))
  (format t "After B~%"))

(defmethod test-2 :after ((object c))
  (format t "After C~%"))

(defmethod test-2 ((object b ))
  (format t "Primary B~%")
  (call-next-method))

(defmethod test-2 ((object c))
  (format t "Primary C~%")
  (call-next-method))

(defmethod test-2 :around ((object c))
  (format t "Around C~%")
  (call-next-method))

(defmethod test-2 :around ((object b))
  (format t "Around B~%")
  (call-next-method))

(defparameter *output* "output")

(defgeneric rebind (obj))

(defmethod rebind :around ((obj a))
  (let ((*output* "new output"))
    (call-next-method)))

(defmethod rebind ((obj a))
  (format t "~a~%" *output*))

(defgeneric priority (obj)
  (:method-combination +))

(defmethod priority + ((obj a))
  (value obj))

(defmethod priority + ((obj b))
  (value obj)
  (call-next-method))

(defun factory (obj)
  (make-instance obj :value 100))

(progn
  (format t "~%")
  (test-2 (factory 'c))
  (format t "~%")
  (rebind (factory 'a))
  (format t "~%")
  (priority (factory 'b)))


(find-method #'test-2 () (list (find-class 'a)))



