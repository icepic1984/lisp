;; Chapter 2.1.1

(defun add-rat (x y)
  (make-rat
   (+
    (* (numer x) (denom y))
    (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat
   (-
    (* (numer x) (denom y))
    (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))


(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defun make-rat (n d)
  (cons n d))

(defun numer (x) (car x))

(defun denom (x) (cdr x))
