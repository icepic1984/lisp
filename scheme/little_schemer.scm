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

(define (subset new old l)
  (cond
   ((null? l) '())
   ((eq? old (car l))
    (cons new (cdr l)))
   (#t (cons (car l) (subset new old (cdr l))))))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (plus1 n m)
  (cond
   ((zero? m) n)
   (else (add1 (plus1 n (sub1 m))))))

(define (mul n m)
  (cond
   ((zero? m) 0)
   (else (plus1 n (mul n (sub1 m))))))

(define (minus1 n m)
  (cond
   ((zero? m) n)
   (else (sub1 (minus1 n (sub1 m))))))

(define (addtub tub)
  (cond
   ((null? tub) 0)
   (else
    (plus1 (car tub) (addtub (cdr tub))))))

(define (mul2 n m)
  (cond
   ((zero? m) 1)
   (else (mul n (mul2 n (sub1 m))))))

(define (tub+ tub1 tub2)
  (cond
   ((null? tub1) tub2)
   ((null? tub2) tub1)
   (else
    (cons (plus1 (car tub1) (car tub2)) (tub+ (cdr tub1) (cdr tub2))))))

(define (gt n m)
  (cond
   ((zero? n) #f)
   ((zero? m) #t)
   (else (gt (sub1 n) (sub1 m)))))

(define (lt n m)
  (cond
   ((zero? m) #f)
   ((zero? n) #t)
   (else (lt (sub1 n) (sub1 m)))))

(define (eq n m)
  (cond
   ((zero? n) (zero? m))
   ((zero? m) #f)
   (else (eq (sub1 n) (sub1 m)))))

(define (div n m)
  (cond
   ((< n m) 0)
   (else (add1 (div (- n m) m)))))

(define (length lat)
  (cond
   ((null? lat) 0)
   (else (add1 (length (cdr lat))))))

(define (pick n lat)
  (cond
   ((zero? n) (car lat))
   (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond
   ((zero? (sub1 n)) (cdr lat))
   (else
    (cons (car lat) (rempick (sub1 n) (cdr lat))))))

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

(subset 'a 'b '(a b c d e))
(subset 'a 'b '())
(subset 'a 'b '(b c))


(add1 10)                               ;11
(sub1 10)                               ;9
(plus1 10 10)                           ;20
(mul 10 3)
(addtub '(10 20 30))

(tub+ '(10 20 30) '(10 20 30))
(tub+ '(10 20 30) '(20 30))
(tub+ '(10 20) '(10 20 30))

(gt 10 2)
(gt 2 10)
(gt 10 10)

(lt 10 2)
(lt 2 10)
(lt 10 10)

(eq 10 2)
(eq 2 10)
(eq 10 10)


