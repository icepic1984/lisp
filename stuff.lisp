
(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (our-remove-if fn (cdr lst))
          (cons (car lst) (our-remove-if fn (cdr lst))))))

(our-remove-if #'evenp '(1 2 3 4 5))

(let ((y 6))
  (defun scope-test (x)
    (list x y)))

(defun scope-test2 (x)
  (list x y))

(let (( y 10))
  (scope-test 8))

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))


(defun make-adder (n)
  #'(lambda (x) (+ x n)))


(setq add10 (make-adder 10)
      add2 (make-adder 2))

(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))

(setq addb (make-adderb 10))

(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val)db) key)
   #'(lambda (key)
       (setf db (delete key db :key #'car)) key)))


(setq cities (make-dbms '((boston . us) (paris . france))))

(funcall (car cities) 'london)

(funcall (second cities) 'london 'england)

(funcall (car cities))


(defun count-instances (obj lsts)
  (labels ((instance-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj)1 0)
                    (instance-in (cdr lst)))
                 0)))
    (mapcar #'instance-in lsts)))


(defun our-length (lst)
  (if (null lst)
      0 (1+ (our-length (cdr lst)))))


(defun our-length (lst)
  (labels ((our-length-priv (lst accum)
             (if (null lst)
                 accum
                 (our-length-priv (cdr lst) (1+ accum)))))
    (our-length-priv lst 1)))

(compiled-function-p #'our-lenght)


(defun explaim (expression)
  (append expression '(oh my)))

(nconc (explaim '(lions and tigers and))'(goddness))

( explaim '(lions and tigers and))

(nconc * '(goodness)
)


(defun b1 ()
  (list 'bla 'blup))

(defun b2 ()
  '(bla blup))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    acc))


(filter #'(lambda ( x) (when (or (= x 10) (= x 20)) x)) '(30 20 30 10 10 20 10))

(find2 #'(lambda ( x) ( = x 10)) '(30 20 30 10 10 20 10))

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))






(defstruct node contents yes no)

(node-yes (make-node :contents 'bla :yes nil :no 'er))

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes  yes
                   :no no)))

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>>" (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n))))
           t (node-contents n)))))

(run-node 'people)
(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male  "Is he living?" 'living 'dead)
(defnode 'dead "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)



(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))


(defmacro our-when (test &body body)
  `(if ,test (progn ,@body)))

(let ((b '(10 20)))
  `(a ,@b c))

(let ((b '(10 20)))
  (cons 'a (append b (list 'c))))


(member 10 '(20 20 10 30 10) :test #'eql)

(memq2 10 '(20 10 20 30))

(defmacro memq2 (x lst)
  `(member ,x ,lst :test #'eql))

(pprint (macroexpand-1 '(or x y)))

(destructuring-bind (x y z) '(a b c) (list x y z))


(defmacro our-dolist ((var list &optional result) &body body)
  `(progn (mapc #'(lambda (,var) ,@body)
           ,list)
    (let ((,var nil))
      ,result)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var ,@body)))

(defmacro when-bind2 (var expr &body body)
  `(let ((,var ,expr))
     (when ,var ,@body)))

(when-bind (x (+ 1 2)) (print x))

(when-bind2 x (+ 1 2) (print x))

(our-dolist (x '(a b c)) (print x))
(our-when t
  (print "he")
  (print "no"))

(nif 1
     (print "pos")
     (print "zero")
     (print "neg"))


(defmacro our-expander (name) `(get ,name 'expander))

(defmacro our-defmacro (name params &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             #'(lambda (,g)
                 (block ,name
                   (destructuring-bind ,params (cdr ,g)
                     ,@body))))
       ',name)))

(our-defmacro our-when (test &body body)
  `(if ,test (progn ,@body)))



(our-expander 'our-whesn)

(do (
     (x 1 (1+ x))
     (y 2 (1+ y))
     (z 3))
    ((> x 10) (princ z) y)
  (princ x)
  (princ y))


(get 'test2 'expander)
    
(our-expander 'test)

(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
            ,label
            (if ,test (return (progn ,@result)))
            ,@body
            (psetq ,@ (make-stepforms bindforms))
            (go ,label))))

(defun make-initforms (bindforms)
  (mapcar #' (lambda (b)
               (if (consp b)
                   (list (car b) (cadr b))
                   (list b nil)))
             bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                  (list (car b) (third b))
                  nil))
          bindforms))
  

(make-stepforms    '( (x 1 (1+ x)) (y 2 (1+ y)) (z 3)))
(make-stepforms '((a b) (c d)))
(make-initforms '((a b) (c d)))
(our-do (
         (x 1 (1+ x))
         (y 2 (1+ y))
         (z 3))
    ((> x 10) (princ z) y)
  (princ x)
  (princ y))


(cadr'(a b c d))

(prog ((x 1) (y 2))
   (print y))


(defmacro our-defun (name params &body body)
  `(progn
     (setf (symbol-function ',name)
           #'(lambda ,params ,@body))',name))

(our-defun our-plus (a b)
  (+ a b))

(our-plus 10 20)

(PROGN
 (SETF SYMBOL-FUNCTION OUR-PLUS
       #'(LAMBDA (A B) (+ A B)))
 OUR-PLUS)

(defmacro our-andb (&rest args)
  (if (null args)
      t
      (labels ((expander (rest)
                 (if (cdr rest)
                     `(if ,(car rest)
                          ,(expander (cdr rest)))
                     (car rest))))
        (expander args))))


(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and ,@ (cdr args))))))
xo

(print (macroexpand  (our-and (= 1 1) (< 1 2) (print "yeah" ))))

(print "yeah")

(declaim (optimize (debug 3)))

(defun test ()
  (loop for x from 1 to 10 do
       (princ "hello")
       (princ x)
       (break)
       (princ "yeah") 
       (princ "adf")))

(loop
   for x below 10
   for y below 10
     collect (+ x y))

(defun fib (n)
  (if (<= 0 n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))


(defclass point ()
  ((x :accessor point-x :initarg :x :initform 0)
   (y :accessor point-y :initarg :y :initform 0)))

