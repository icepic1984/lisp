(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (lat? l)
  (cond
   ((null? l) #t)
   ((atom? (car l)) (lat? (cdr l)))
   (else #f)))


