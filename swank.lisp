(defmacro continuable (&body body)
  `(restart-case (progn ,@body)
     (continue () :report "FUCK")))

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
                         (swank::default-connection))))
     (when connection (swank::handle-requests connection t)))))

(defparameter *running* t)

(defun toggle-running ()
  (if *running* (setq *running* nil)
      (setq *running*  t)))

(defun rundemo ()
    (let ((counter 0))
      (loop while *running* do
           (when (> 100000 counter)
             (setq counter 0)
             (print counter))
           (update-swank)
           (sleep 1/10)
           (incf counter))))

(toggle-running)





