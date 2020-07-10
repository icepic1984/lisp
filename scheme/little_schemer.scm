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
   ((member? (car lat) (cdr lat)) #f)
   (else (set? (cdr lat)))))

(define (makeset lat)
  (cond
   ((null? lat) '())
   ((member? (car lat) (cdr lat))
    (makeset (cdr lat)))
   (else (cons (car lat) (makeset (cdr lat))))))

(define (subset? set1 set2)
  (cond
   ((null? set1) #t)
   ((member? (car set1) set2) (subset? (cdr set1) set2))
   (else #f)))

(define (eqset? set1 set2)
  ((and (subset? set1 set2)
        (subset? set2 set1))))

(define (intersect? set1 set2)
  (cond
   ((null? set1) #f)
   ((member? (car set1) set2) #t)
   (else (intersect? (cdr set1) set2))))

(define (intersect set1 set2)
  (cond
   ((null? set1) '())
   ((member? (car set1) set2)
    (cons (car set1) (intersect (cdr set1) set2)))
   (else
    (intersect (cdr set1) set2))))

(define (union set1 set2)
  (cond
   ((null? set1) set2)
   ((member? (car set1) set2)
    (union (cdr set1) set2))
   (else
    (cons (car set1) (union (cdr set1) set2)))))

(define (intersectall l)
  (cond
   ((null? (cdr l)) (car l))
   (else (intersect (car l) (intersectall (cdr l))))))

(define (a-pair? x)
  (cond
   ((atom? x) #f)
   ((null? x) #f)
   ((null? (cdr x)) #f)
   ((null? (cdr (cdr x))) #t)
   (else #f)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (third p)
  (car (cdr (cdr p))))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (fun? rel)
  (set? (firsts rel)))

(define (revrel rel)
  (cond
   ((null? rel) '())
   (else
    (cons
     (build (second (car rel)) (first (car rel)))
     (revrel (cdr rel))))))

(define (rember-f test? a l)
  (cond
   ((null? l) '())
   (else
    (cond
     ((test? (car l) a) (cdr l))
     (else
      (cons (car l) (rember-f test? a (cdr l))))))))

(define (rember-f test?)
  (lambda (a l)
    (cond
     ((null? l) '())
     ((test? (car l) a) (cdr l))
     (else
      (cons (car l) ((rember-f test?) a (cdr l)))))))


(define (insertL-f test?)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((test? (car l) old)
      (cons new (cons old (cdr l))))
     (else
      (cons (car l) ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond 
     ((null? l) '())
     ((test? (car l) old)
      (cons old (cons new (cdr l))))
     (else
      (cons (car l) ((insertR-f test?) new old (cdr l)))))))

(define (left new old l)
  (cons new (cons old (cdr l))))

(define (right new old l)
  (cons old (cons new (cdr l))))

(define (insert-g test? side)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((test? (car l) old)
      (side new old (cdr l)))
     (else
      (cons (car l) ((insert-g test? side) new old (cdr l)))))))


(define (rember-new a l)
  ((insert-g equal? (lambda (new old l) l)) #f a l))


(define (multiremberT test? lat)
  (cond
   ((null? lat) '())
   ((test? (car lat))
    (multiremberT test? (cdr lat)))
   (else
    (cons (car lat) (multiremberT test? (cdr lat))))))


(define multiremberCo
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multiremberCo a (cdr lat)
                     (lambda (newlat seen)
                       (col newlat (cons (car lat) seen)))))
     (else
      (multiremberCo a (cdr lat) (lambda (newlat seen)
                                   (col (cons (car lat) newlat)
                                        seen)))))))


(define (multiinsertL new old lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) old)
    (cons new (cons old (multiinsertL new old (cdr lat)))))
   (else
    (cons (car lat) (multiinsertL new old (cdr lat))))))

(define (multiinsertR new old lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) old)
    (cons old (cons new (multiinsertR new old (cdr lat)))))
   (else
    (cons (car lat) (multiinsertR new old (cdr lat))))))


(define (multiinsertLR new oldL oldR lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) oldL)
    (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
   ((eq? (car lat) oldR)
    (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
   (else
    (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))


(define (multiinsertLR&co new oldL oldR lat col)
  (cond
   ((null? lat) (col '() 0 0))
   ((eq? (car lat) oldL)
    (multiinsertLR&co
     new oldL oldR (cdr lat) (lambda (newlat L R)
                               (col (cons new (cons oldL newlat))
                                    (add1 L) R))))
   ((eq? (car lat) oldR)
    (multiinsertLR&co
     new oldL oldR (cdr lat) (lambda (newlat L R)
                               (col (cons oldR (cons new newlat)) L
                                    (add1 R)))))
   (else
    (multiinsertLR&co new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons (car lat) newlat) L R))))))

(multiinsertLR&co 'salty 'fish 'chips
                  '(chips and fish or fish and chips)
                  (lambda (lat l r)
                    (display lat)))


(define (a-friend x y)
  (length y))

(define (keep-looking a sorn lat)
  (cond
   ((number? sorn) (keep-looking a (pick sorn lat) lat))
   (else
    (eq? sorn a))))


(define (eternity x)
  eternity x)

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else
         (add1 (length (cdr l))))))) eternity)) '(2))

(((lambda (mk-length)
    (mk-length (mk-length (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))) '(2 3))


(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (add1 (length (cdr l)))))))) '())

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length) (cdr l)))))))) '(3 2))


(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
     (lambda (x)
       ((mk-length mk-length) x))))) '(d d))


(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

((Y (lambda (length)
      (lambda (l)
        (cond
         ((null? l ) 0)
         (else (add1 (length (cdr l)))))))) '(a b))



(define (bla length)
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

((Y bla) '(a b))


'((a b) (c d))

(build '(a a) '(c d))


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry
            name (car table)
            (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

(define new-entry build)

(define extend-table cons)

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))


(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote)
        *qoute)
       ((eq? (car e) 'lambda)
        *lambda)
       ((eq? (car e) 'cond)
        *cond)
       (else *application)))
     (else *application))))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define value
  (lambda (e)
    (meaning e '())))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else
      (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(define question-of first)

(define answer-of second)

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#f)) ((klatsch party) (5 (6)))))

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))


(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))

(define function-of car)
(define arguments-of cdr)

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e ) table)
     (evlis (arguments-of e) table))))

(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))

(*identifier 'dfdf '(()))

(*quote '(quote df) '())



(lookup-in-table 'luise
                 '(((harald) (schmidt))
                   ((hasn luise) (meyer smidth)))
                 (lambda (name) name))

(keep-looking 'test 0 '(1 2 3 test))

(multiremberCo 'tuna '(straberries tuna and swordfisch) a-friend)

(multiremberCo 'tuna '(tuna) a-friend)

((insertL-f equal? ) 'a 'b '(b c d e f))
((insertR-f equal? ) 'a 'b '(b c d e f))

(rember-new 'b '(a b c d))

((insert-g equal? left) 'a 'b '(b c d e f))
((insert-g equal? right) 'a 'b '(b c d e f))

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
