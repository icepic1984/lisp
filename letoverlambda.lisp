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
(defun sleep-units% (value unit)
  (sleep
   (* value
      (case unit
        ((s) 1)
        ((m) 60)
        ((h) 3600)
        ((d) 86400)
        ((ms) 1/1000)
        ((us) 1/1000000)))))

(sleep-units% 10 's)

(defmacro sleep-units (value unit)
  `(sleep
    (* , value
         ,(case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000)))))

(sleep-units 10 s)

(defmacro unit-of-time (value unit)
  `(* , value
       ,(case unit
          ((s) 1)
          ((m) 60)
          ((h) 3600)
          ((d) 86400)
          ((ms) 1/1000)
          ((us) 1/1000000))))

;; See lol 45
;; Macro which let us use named let from scheme
(mapcar #'car '((a b c) (d e f)))
(mapcar #'cadr '((a b c) (d e f)))

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@ (mapcar #'cadr letargs))))

(defun nlet-fac (n)
  (nlet fact ((n n))
        (if (zerop n)
            1
            (* n (fact (- n 1))))))

(macroexpand  '(nlet fact ((n n))
                    (if (zerop n)
                        1
                        (* n (fact (- n 1))))))


;; See lol 57
(defmacro nif% (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g), pos)
             ((zerop ,g), zero)
             (t, neg)))))

(nif% (+ 1 -10) 'pos 'zero 'neg)

;; See lol 59
;; Test if symbol is a gensym symbol (start with G!)
(defun g!-symbol-p (s)
  (print s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "G!" :start1 0 :end1 2 )))

;; See lol 60
(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq (symbol-name s)
                                      2))))
              syms)
         ,@body))))

;; Example
(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result), pos)
           ((zerop ,g!result), zero)
           (t ,neg))))

(nif (+ -1 10) 'pos 'zero 'neg)

(g!-symbol-p ',g!-b)
(remove #\, ",GResul")
