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

(define (no-nums lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((number? (car lat)) (no-nums (cdr lat)))
     (else
      (cons (car lat) (no-nums (cdr lat))))))))

(define (all-nums lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat)))))))

(define (eqan? a1 a2)
  (cond
   ((and (number? a1 ) (number? a2))
    (= a1 a2))
   ((or (number? a1) (number? a2)) #f)
   (else (eq? a1 a2))))

(define (occur a lat)
  (cond
   ((null? lat) 0)
   (else
    (cond
     ((eq? (car lat) a)
      (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat)))))))

(define (one? n)
  (cond
   ((zero? n) #f)
   (else (zero? (sub1 n)))))


(define (rember* a l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((eq? (car l) a) (rember* a (cdr l)))
     (else
      (cons (car l) (rember* a (cdr l))))))
   (else (cons (rember* a (car l)) (rember* a (cdr l))))))


(define (occur* a l)
  (cond
   ((null? l) 0)
   ((atom? (car l))
    (cond
     ((eq? (car l) a) (add1 (occur* a (cdr l))))
     (else (occur* a (cdr l)))))
   (else (+ (occur* a (car l) )
            (occur* a (cdr l))))))

(define (member* a l)
  (cond
   ((null? l) #f)
   ((atom? (car l))
    (or (eq? (car l) a)
        (member* a (cdr l))))
   (else (or (member* a (car l))
             (member* a (cdr l))))))


(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((and (null? l1) (atom? (car l2))) #f)
   ((null? l1) #f)))


(define (numbered? a)
  (cond
   ((atom? a) (number? a))
   (else
    (and (numbered? (car a))
         (numbered? (car (cdr (cdr a))))))))

(define (value exp)
  (cond
   ((atom? exp) exp)
   ((eq? (car (cdr exp)) '+)
    (+ (value (car exp)) (value (car (cdr (cdr exp))))))
   ((eq? (car (cdr exp)) 'x)
    (* (value (car exp)) (value (car (cdr (cdr exp))))))))

(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (set? lat)
  (cond
   ((null? lat) #t)
   (else
    (cond
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat)))))))

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



(occur* 'a '(a (a (b c d (a) )) b (a c)))

(occur* 'a '((a b) (a c)))
