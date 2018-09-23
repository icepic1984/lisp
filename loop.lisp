(use-package :cl-ppcre)

(defparameter *file* (pathname "/home/icepic/Code/lisp/test.xml"))

(defparameter *refxy* (create-scanner ".*?ReferenzXY value = \"([0-9,]+)\".*"))
(defparameter *refz* (create-scanner ".*?ReferenzZ value = \"([0-9,]+)\".*"))
(defparameter *refMesh* (create-scanner ".*?ReferenzMesh value = \"([0-9,]+)\".*"))


(defun match-regexp (line regexp)
  (register-groups-bind (value)
      (regexp line :sharedp t ) value))

(defun apply-on-line-until (func line regexps)
  (loop for regex in regexps thereis (funcall func line regex)))

(defun retrieve-values-from-file (file &rest regexp)
  (with-open-file (stream file)
    (loop
       for line = (read-line stream nil) while line
       for matched = (apply-on-line-until #'match-regexp line regexp)
       when matched collect it)))

(retrieve-values-from-file *file* *refxy* *refz* *refmesh*)





  
