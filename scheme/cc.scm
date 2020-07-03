
(define x (call/cc (lambda (k) k)))

(define x  (call/cc (lambda (k) (k 0))))

(+ (* 1 2) 3)

(+ (call/cc (lambda (k) (* 1 2))) 3)

(+ (call/cc (lambda (k) (k (* 1 2)))) 3)

(define z #f)
(+ (call/cc (lambda (k) (set! z k) (* 1 2))) 3)
(z 30)

(let ((x (call/cc (lambda (k) k))))
  (x (lambda (o) "hi")))


(let ((x (call/cc (lambda (k) k))))
  (x (lambda (o) "hi")))

((lambda (v) (v (lambda (o) "hi"))) (lambda (o) "hi"))

(define call/cc call-with-current-continuation) ;  1
                                                ;  2
(define f #f)                                   ;  3
                                                ;  4
(define-syntax amb                              ;  5
  (syntax-rules ()                              ;  6
    ((_) (f))                                   ;  7
    ((_ a) a)                                   ;  8
    ((_ a b ...)                                ;  9
     (let ((s f))                               ; 10
       (call/cc                                 ; 11
        (lambda (k)                             ; 12
          (set! f (lambda ()                    ; 13
                    (set! f s)                  ; 14
                    (k (amb b ...))))           ; 15
          (k a)))))))                           ; 16
                                                ; 17
(define (really? x y)                           ; 18
  (if (equal? x y)                              ; 19
      (list x y)                                ; 20
      (amb)))                                   ; 21
                                                ; 22
(call/cc                                        ; 23
 (lambda (k)                                    ; 24
   (set! f (lambda ()                           ; 25
             (k 'no-choices)))))                ; 26
