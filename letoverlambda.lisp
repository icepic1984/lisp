;; Disable compiler optimization
(declaim (optimize (speed 0) (space 0) (debug 3)))

;; See Onlisp 47
;; Group list into sublist of size n
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))
                   ))))
    (if source (rec source nil) nil)))

(group '(a b c d e f g) 2)


;;See Onlisp 49
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(flatten (group '(a b c d e f g) 2))

;; See Onlisp 58

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; See Lol 15
;; Calculate factorial
(defun fact (x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

;; See lol 15
;; binominal coefficient
(defun choose (n r )
  (/ (fact n)
     (fact (- n r))
     (fact r)))



;; Lexcial scoping vs dynamic scoping
;; Dynamic scoping
(defvar temp-special)
(setq temp-special 1)
(defun temp-special-returner ()
  temp-special)

(let ((temp-special 2))
  (temp-special-returner))

;; Lexical scoping
(setq temp-lexical-2 2)
(defun temp-lexical-returner ()
  temp-lexical-2)
(let ((temp-lexical-2 3))
  (temp-lexical-returner))

;; See lol 35
(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list)))
        (dolist (c data)
          (if curr
              (setq curr
                    (if (char= (car curr) c)
                        (cdr curr)
                        trig)))))
      (not curr))))

(defvar scanner (block-scanner "jihad"))

(funcall scanner "we will start")
(funcall scanner "the ji")
(funcall scanner "had tomorrow.")

;; See lol 36
;; let over lambdas as objects with static data
(let ((direction 'up))
  (defun toggle-counter-direction ()
    (setq direction
          (if (eq direction 'up)
              'down
              'up)))

  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
        (if (eq direction 'up)
            (incf counter)
            (decf counter))))))

(defvar counter (counter-class))
(funcall counter)
(toggle-counter-direction)


;; See lol 41
