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


(define-condition food-error (error) ())

(define-condition bad-tasting-sundae (food-error)
  ((ice-cream :initarg :ice-cream :reader bad-tasting-sundae-ice-cream)
   (sauce :initarg :sauce :reader bad-tasting-sundae-sauce)
   (topping :initarg :topping :reader bad-tasting-sundae-topping))
  (:report (lambda (condition stream)
             (format stream "Bad tasting sundae with ~S, ~S, and ~S"
                     (bad-tasting-sundae-ice-cream condition)
                     (bad-tasting-sundae-sauce condition)
                     (bad-tasting-sundae-topping condition)))))

(defun all-start-with-same-letter (symbol1 symbol2 symbol3)
   (let ((first-letter (char (symbol-name symbol1) 0)))
     (and (eql first-letter (char (symbol-name symbol2) 0))
          (eql first-letter (char (symbol-name symbol3) 0)))))

(defun read-new-value ()
   (format t "Enter a new value: ")
   (multiple-value-list (eval (read))))

(defun verify-or-fix-perfect-sundae (ice-cream sauce topping)
   (do ()
      ((all-start-with-same-letter ice-cream sauce topping))
     (restart-case
       (error 'bad-tasting-sundae
              :ice-cream ice-cream
              :sauce sauce
              :topping topping)
       (use-new-ice-cream (new-ice-cream)
         :report "Use a new ice cream."
         :interactive read-new-value  
         (setq ice-cream new-ice-cream))
       (use-new-sauce (new-sauce)
         :report "Use a new sauce."
         :interactive read-new-value
         (setq sauce new-sauce))
       (use-new-topping (new-topping)
         :report "Use a new topping."
         :interactive read-new-value
         (setq topping new-topping))))
  (values ice-cream sauce topping))



(defun bla (i)
  (catch :bla
    (when i
      (format t "Before bla~%")
      (error "error~%")
      (format t "After bla~%")))
  (format t "End bla~%"))


(handler-case
 (bla t)
  (error () (throw :bla nil)))

(handler-bind
    ((error (lambda (c)
              (format t "Error~%")
              (throw :bla nil))))
  (format t "Before~%")
  (bla t)
  (format t "After~%"))

(defun blup ()
  (format t "Blup~%")
  (catch 'bla
    (bla t)
    (format t "Blup 2~%")))


