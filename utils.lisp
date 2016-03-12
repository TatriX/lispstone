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
   (make-pathname :directory '(:relative "assets") :name name :type :unspecific)
   (if *debug*
       (asdf/system:system-source-directory :lispstone)
       *default-pathname-defaults*)))


(defmacro with-fields (slots instance &body body)
  `(with-accessors ,(loop for v in slots collect `(,v ,(symbolicate instance "-" v))) ,instance
    ,@body))


(defun tt (fmt &rest args)
  (i18n fmt :params args))
