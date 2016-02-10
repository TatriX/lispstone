(in-package :lispstone)

(defun prompt-read (prompt)
  (format *query-io* "~&~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


(defun roll-dice ()
  (random 100))

(defun lucky (value)
  (< (roll-dice) value))

(defun asset-path (name)
  (merge-pathnames
   (concatenate 'string "assets/" name)
   (if *debug*
       (asdf/system:system-source-directory :lispstone)
       *default-pathname-defaults*)))
