(define-condition ask-for-name () ()
  (:documentation "A mock coniditon that means a name needs to be provided"))

(defun name (user)
  (car user))

(defun friend-names (user)
  (cdr user))

(defun (setf friend-names) (new-value user)
  (setf (cdr user) new-value))


;;; A helper macro wrapping an idiom allowing to do a test and request a new value if test fails.
(defmacro ensure (test place condition)
  "If the `TEST' fails, signal a `CONDITION', with a restart `USE-VALUE' ready for a new value."
  `(restart-case
       (unless ,test
         (signal ,condition))
     (use-value (new-value)
       (setf ,place new-value))))


(defun get-name (user)
  (let ((name (name user)))
    (ensure (not (null name)) ;Just NAME would suffice, but spelling it out for clarity.
            name
            'ask-for-name)
    name))

(defun make-friends (user-1 user-2)
  (push (get-name user-2) (friend-names user-1))
  (push (get-name user-1) (friend-names user-2)))


(let ((ricky (cons nil nil))
      (bubbles (cons "Bubbles" nil)))
  (handler-bind ((ask-for-name (lambda (c) (use-value "Ricky" c))))
    (make-friends ricky bubbles)
    ;; Let's return the two objects for the sake of REPL output.
    (list ricky bubbles)))


(define-condition progress ()
  ((amount :initarg :amount :reader amount)))

(defun process-partial-data (data)
  "NOOP placeholder"
  (declare (ignore data))
  (sleep 2))

(defun process-data (data)
  (format t "Start process ~%")
  (restart-case
      (loop
        initially
           (signal 'progress :amount 0)
        with total = (length data)
        for datum in data
        for i below total
        do
           (process-partial-data datum)
           (signal 'progress :amount (/ i total))
           ;; Report progress
        finally
           (signal 'progress :amount 1)
           (return :done))
    (abort-work ()
      (format *trace-output* "Aborting work!")
      :failed)))


(handler-case (process-data '(1 2 3))
  (progress (e) (format t "yeah ~A ~%" (amount e))))


(defparameter *abort-by-user* nil )

(setf *abort-by-user* t)

(handler-bind
    ((progress
       (lambda (p)
         (format *trace-output* "~&Progress: ~F~%" (amount p))
         (when *abort-by-user*
           (invoke-restart 'abort-work)))))
  (process-data '(1 2 3)))

