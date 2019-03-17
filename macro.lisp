
(macroexpand-1 (ffi:def-enum test (:a :b :c)))


(defmacro def-enum2 (name values-list &key (separator-string "#"))
  (let ((constants '())
        (value -1)
        field
        forms)
    (setf 
          separator-string (string separator-string))
    (dolist (item values-list)
      (cond ((symbolp item)
             (setf field item)
             (incf value))
            ((and (consp item)
                  (symbolp (setf field (first item)))
                  (integerp (setf value (second item)))
                  (endp (cddr item))))
            (t
             (error "Not a valid argument to DEF-ENUM~%~a" values-list)))
      (setf field (concatenate 'string
                               (symbol-name name)
                   xo            separator-string
                               (string field)))
      (push `(defconstant ,(intern field (symbol-package name))
               ',value)
            forms))
    `(progn
       (ffi:def-foreign-type ,name :int)
       ,@forms)))


(setq x '(PROGN
          (FFI:DEF-FOREIGN-TYPE TEST2 :INT)
          (DEFCONSTANT |TEST2#C| '2)
          (DEFCONSTANT |TEST2#B| '1)
          (DEFCONSTANT |TEST2#A| '0)))


(defmacro def-enum3 (name values-list &key (separator-string "#"))
  `(to-enum `(ffi:def-enum ,name ,values-list )))

(defmacro def-enum4 (name values-list)
  `(to-enum (macroexpand (ffi:def-enum ,name ,values-list))))

(defmacro enum-to-string (enum)
  (let ((value)
        (str)
        (forms))
    (dolist (item (cddr (macroexpand enum)))
      (setf value (third item))
      (setf str (string (second item)))
      (push `(,value ,str) forms))
    `(defun te (value) (ecase value ,@forms))))

(enum-to-string (FFI:DEF-ENUM TEST4 (:A :B :C)))

(te 2)


