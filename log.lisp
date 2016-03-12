(in-package :lispstone)

(defparameter *trace-enabled* t)

(defclass round-log ()
  ((records :initform '()
            :accessor log-records)))

(defvar *round-log* (make-instance 'round-log))

(defun clear-log (log)
  (setf (log-records log) nil))

(defun push-to-round-log (fmt &rest args)
  (with-slots (records) *round-log*
    (push (apply #'tt fmt args) records)))

(defun log-trace (fmt &rest args)
  (when *trace-enabled*
    (apply #'format *standard-output* fmt args)))

(defun push-and-trace (fmt &rest args)
  (apply #'push-to-round-log fmt args)
  (apply #'log-trace fmt args))
