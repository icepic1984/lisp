;; See https://stackoverflow.com/questions/45665951/in-common-lisp-when-do-you-need-to-use-eval-when-and-how-do-you-know
;; https://stackoverflow.com/questions/10674650/eval-when-uses

(defparameter *load-state* nil)
(defparameter *compile-state* nil)
(defparameter *execute-state* nil)


(eval-when (:load-toplevel)
  (setf *load-state* "eval") 
  (format t "Load~%"))

(eval-when (:compile-toplevel)
  (setf *compile-state* "compile") 
  (format t "Compile~%"))

(eval-when (:execute)
  (setf *execute-state* "execute")
  (format t "Execute~%"))

(progn
  (format t "Load    Status: ~s~%" *load-state*)
  (format t "Compile Status: ~s~%" *compile-state*)
  (format t "Execute Status: ~s~%" *execute-state*))

;; Following snippet shows why eval-when is needed, if a macro calls a
;; helper function during compilation and this macro is used inside
;; the same function. The following snipped will not compile!
;; 
;; Function run-at-compile-time is compiled but
;; not evaluated during compile time
(defun run-at-compile-time ()
  (print 'I-am-called-at-compile-time))

;; In macro foo we are calling run-at-compile-time
(defmacro foo ()
  (run-at-compile-time)
  '(print 'I-am-called-at-runtime))

;; Expansion of foo is triggered. (Is commented out, so the file
;; compiles in order to run the working example).
;; 
;; (foo)

;; To make it compile, run-at-compile-time needs to be wrapped with
;; eval-when, so that compile-time environment is aware of its
;; function definition.
(eval-when (:compile-toplevel)
  (defun run-at-compile-time-2 ()
    (print 'I-am-called-at-compile-time-2)))

;; In macro foo we are calling run-at-compile-time
(defmacro foo-2 ()
  (run-at-compile-time-2)
  '(print 'I-am-called-at-runtime-2))

;; Expansion of foo is triggered
(foo-2)

