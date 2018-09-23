(defparameter file (pathname "/home/icepic/dump/test.lisp"))

(defun match (line)
  (when (< 5 (random 10))line))

(with-open-file (stream file)
  (loop
     for line = (read-line stream nil) while line
     for matched = (apply-until #'match line) when matched collect matched))

(defun match-line (reg)
    (when (= 10 reg ) reg))

(defun apply-until (pred list)
  (loop for item in list thereis (funcall pred item)))

(defun test3 ( &rest bla)
  (apply-until #'match-line bla))

(test3 20 30 40 10)




  
