(defparameter *nodes* '((living-room (you are in the livivng-room.
                                      a wizard is snoring loudly on the couche.))
                        (garden (you are in a beautiful garden. there is a well in front of you.))
                        (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                         (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (torch attic)
                                   (frog garden)))

(defparameter *location* 'living-room)

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
    (if next (progn (setf *location* (car next))
                    (look))
        '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (object-at 'body *objects* *object-locations*)))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x) (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look pickup walk inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know the command)))

(defun tweak-test (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))ppp
      (cond ((eq item #\space) (cons item (tweak-test rest caps lit)))
            ((member item '(#\? #\! #\.)) (cons item (tweak-test rest t lit)))
            ((eq item #\") (tweak-test rest caps (not lit)))
            (lit (cons item (tweak-test rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-test rest nil lit)))
            (t (cons (char-downcase item) (tweak-test rest nil nil )))))))

(defun game-print (lst)
  (princ (coerce (tweak-test (coerce (string-trim "() " (prin1-to-string lst))'list)t nil)
                 'string)) (fresh-line))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))



(time (dotimes (i 1000) (game-print '(adfdf dfdf dfd fdf ))))
