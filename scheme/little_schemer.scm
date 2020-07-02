(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (lat? l)
  (cond
   ((null? l) #t)
   ((atom? (car l)) (lat? (cdr l)))
   (else #f)))

(define (member? a lat)
  (cond
   ((null? lat) #f)
   (else
    (or (eq? a (car lat))
        (member? a (cdr lat))))))

(define (rember a lat)
  (cond
   ((null? lat) '())
   ((eq? a (car lat))
    (cdr lat))
   (else (cons (car lat) (rember a (cdr lat))))))

(define (firsts l)
  (cond
   ((null? l) '())
   (else
    (cons (car (car l)) (firsts (cdr l))))))

(define (insertR new old l)
  (cond
   ((null? l) '())
   ((eq? old (car l)) (cons old (cons new (cdr l))))
   (#t (cons (car l) (insertR new old (cdr l))))))

(define (insertL new old l)
  (cond
   ((null? l) '())
   ((eq? old (car l)) (cons new (cons old (cdr l))))
   (#t (cons (car l) (insertL new old (cdr l))))))

(atom? 'a)
(atom? '())
(atom? '(a b))

(lat? '())
(lat? '(a b c))
(lat? '((a b) c))

(member? 'a '(a b c))
(member? 'd '(a b c))
(member? 'a '())

(rember 'a '(a b c))
(rember 'a '(a a b c))
(rember 'a '(b a c d))
(rember 'a '(b c d))
(rember 'a '())

(firsts '((a b c) (a b c) (a b c)))
(firsts '())

(insertL 'a 'b '(a b c d e))
(insertR 'a 'b '(a b c d e))

