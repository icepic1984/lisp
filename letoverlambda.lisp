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






