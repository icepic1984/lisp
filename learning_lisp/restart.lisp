;; Signals error, which will return if no handler is found
(defun real-sqrt-signal (n)
  (when (minusp n)
    (setq n (- n))
    (signal 'error))
  (sqrt n))

(real-sqrt-signal -10)

(handler-bind ((error (lambda (c)
                        (declare (ignore c))
                        (format t "Error~%"))))
  (real-sqrt-signal -10))


;; Error: if no handler found, throw into debugger.
(defun real-sqrt-error (n)
  (when (minusp n)
    (restart-case
        (error "error")
      (continue () :report "Continue" nil)))
  (sqrt n))

(real-sqrt-error -10)


(handler-bind ((error (lambda (c)
                        (declare (ignore c))
                        (format t "Error~%")
                        (invoke-restart 'continue))))
  (real-sqrt-error -10))

;; As cerror but establish continue restart handler
(defun real-sqrt-cerror (n)
  (when (minusp n)
    (cerror "Ignore" "Negative value"))
  (sqrt n))

(real-sqrt-cerror -10)

;; Manual invoke continue
(handler-bind ((error (lambda (c)
                        (declare (ignore c))
                        (format t "Error~%")
                        (invoke-restart 'continue))))
  (real-sqrt-cerror -10))

;; Use lisp helper
(handler-bind ((error (lambda (c)
                        (declare (ignore c))
                        (format t "Error~%")
                        (continue))))
  (real-sqrt-cerror -10))

;; If no handler found, print warn to stderror
(defun real-sqrt-warn (n)
  (when (minusp n)
    (warn "Negative value"))
  (sqrt n))

(real-sqrt-warn -10)

(handler-bind ((warning (lambda (c)
                          (declare  (ignore c))
                          (muffle-warning))))
     (real-sqrt-warn -10))

