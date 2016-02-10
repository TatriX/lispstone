(in-package :lispstone)

(defparameter *deck-size* 30)

(defun random-deck ()
  (loop for i below *deck-size*
     for prototype = (random-elt *cards*)
     collect (make-instance 'card
                            :name (card-name prototype)
                            :cost (card-cost prototype)
                            :desc (card-desc prototype)
                            :effect (card-effect prototype))))
