
(defgeneric test (obj))


(defmethod test ((obj list))
  (format t "list"))

(defmethod test ((obj integer))
  (format t "integeer"))
