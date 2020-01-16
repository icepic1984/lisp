
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


(defparameter *account* (make-instance 'bank-account :balance 100))

(defparameter *backup* (make-instance 'checking-account :overdraft-account *account* :balance 120))

(balance *account*)
(balance *backup*)

(withdraw *backup* 130)
