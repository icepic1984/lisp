(defmacro enum-to-string (name enum)
  (let ((value)
        (str)
        (forms))
    (dolist (item enum)
      (setf value (third item))
      (setf str (string (second item)))
      (push `(,value ,str) forms))
    `(defun ,name (value) (ecase value ,@forms))))

(defmacro def-enum (name values-list &key (separator-string "#"))
  (let ((constants '())
        (value -1)
        field
        convert
        forms)
    (setf separator-string (string separator-string))
    (dolist (item values-list)
      (cond ((symbolp item)
             (setf field item)
             (incf value))
            ((and (consp item)
                  (symbolp (setf field (first item)))
                  (integerp (setf value (second item)))
                  (endp (cddr item))))
            (t (error "Not a valid argument to DEF-ENUM~%~a" values-list)))
      (setf field (concatenate 'string
                               (symbol-name name)
                               separator-string
                               (string field)))
      (push `(defconstant ,(intern field (symbol-package name))
               ',value) forms))
    (setf convert (concatenate 'string (string-upcase (symbol-name name)) "-TO-STRING" ))
    (push `(enum-to-string ,(intern convert) ,forms) forms )
    `(progn
       (ffi:def-foreign-type ,name :int)
       ,@forms)))

(DEF-ENUM test4 (:A :B :C :f) :separator-string "-")



