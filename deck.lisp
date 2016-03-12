(in-package :lispstone)

(defparameter *deck-size* 30)

(defun random-deck (cards)
  (loop for i below *deck-size*
     collect (random-card cards)))
