
(defparameter *the-false-value* nil)

(defun truep (value)
  (not (eq *the-false-value* value)))

(defun booleanp (value)
  (or (eq *the-false-value* value)
      (eq t value)))


(defparameter *empty-progn* 69)

(defun evaluate-progn (body env)
  (if (consp body)
      (if (consp (cdr body))
          (progn (evaluate (car body) env)
                 (evaluate-progn (cdr body) env))
          (evaluate (car body) env))
      *empty-progn*))

(defparameter *initial-environment* '())

(defun lookup (id env)
  (if (consp env)
      (if (eq (car (car env)) id)
          (cdr (car env))
          (lookup id (cdr env)))
      (error "no such binding ~s" id)))

(defun update! (id env value)
  (if (consp env)
      (if (eq (car (car env)) id)
	      (progn (rplacd (car env) value)
		         value)
	      (update! id (cdr env) value))
      (error "no such binding ~s" id)))

(defun extend (env variables values)
  (cond ((consp variables)
	     (if (consp values)
	         (cons (cons (car variables)
			             (car values))
		           (extend env
			               (cdr variables)
			               (cdr values)))
	         (error "too few values")))
	    ((null variables)
	     (if (null values)
	         env
	         (error "too many values")))
	    ((symbolp variables)
	     (cons (cons variables values) env))))

(defun evaluate (e env)
  (if (atom e)
      (if (symbolp e)
          (lookup e env)
          e)
      (case (car e)
        ((quote) (cdr e))
        ((if) (if (truep (evaluate (cadr e) env))
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env)))
        ((progn) (evaluate-progn (cdr e) env))
        ((setq) (update! (car (cdr e)) env
                         (evaluate (car (cdr (cdr e))) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (t (invoke (evaluate (car e ) env )
                   (evlis (cdr e ) env))))))


(defun invoke (function args)
  (if (functionp function)
      (funcall function args)
      (error "not a function ~s" function)))

(defun make-function (variables body env)
  (lambda (values)
    (evaluate-progn body (extend env variables values))))

(defun evlis (exps env)
  (if (consp exps)
      (cons (evaluate (car exps) env)
            (evlis (cdr exps) env))))


(defparameter *global-environment*
  *initial-environment*)

(defmacro definitial (name &optional (value nil value-supplied-p))
  `(progn (push (cons (quote ,name)
		              ,(if value-supplied-p
			               value
			               ''void))
		        *global-environment*)))

(defmacro defprimitive (name value arity)
  `(definitial ,name
       (lambda (values)
	 (if (= ,arity (length values))
	     (apply (function ,value) values)
	     (error "Incorrect arity ~s"
		    (list (quote ,name) values))))))

(progn
  (defprimitive cons cons 2)
  (defprimitive car car 1)
  (defprimitive rplacd rplacd 2)
  (defprimitive eq eq 2)
  (defprimitive list list 2))

(progn
  (defprimitive + + 2)
  (defprimitive < < 2))


(definitial t t)
(definitial f *the-false-value*)
(definitial nil '())

(defun chapter1-scheme ()
  (labels ((toplevel ()
	     (let ((value (read)))
	       (unless (eq :exit value)
		 (print (evaluate value *global-environment*))
		 (terpri)
		 (toplevel)))))
    (toplevel)))


(invoke (evaluate '(lambda (a b c) b) *initial-environment*) '(10 20 30))

(invoke  (evaluate '(lambda (x) (if x 10 20)) *global-environment*) '(nil))

(evaluate '(if t 2 3) *global-environment*)

(evaluate '(quote e) *global-environment*)

(evaluate '(list 1 2) *global-environment* )

(evaluate '((lambda (a)
              ((lambda (b)
                 (list a b))
               (+ 2 a))) 1) *global-environment*)

(truep (evaluate nil *initial-environment*))
