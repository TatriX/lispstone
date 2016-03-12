(in-package :lispstone)

(defclass hand-entity (entity)
  ((hand :initarg :hand)))

(defparameter *card-width* 96)
(defparameter *card-height* 128)

(defun hand-entity-cards-num (entity)
  (length (hand-cards (slot-value entity 'hand))))

(defmethod entity-width ((entity hand-entity))
    (* *card-width* (hand-entity-cards-num entity)))

(defmethod entity-height ((entity hand-entity))
  *card-height*)

(defmethod update-entity ((entity hand-entity) scene)
  (with-slots (id x y hand children (hand-anchor-y anchor-y)) entity
    (unless (= (length children) (hand-entity-cards-num entity))
      (setf children
            (with-slots (cards) hand
              (loop for card in cards
                 with full-width = (entity-width entity)
                 with dy = y
                 for card-entity = (ecase id
                                     (:player-hand (clone-card-entity (get-card-entity card) card))
                                     (:enemy-hand (make-card-back-entity)))
                 for dx = (- x (ash full-width -1)) then (+ dx *card-width*)
                 do
                   (with-slots (x y anchor-y) card-entity
                     (setf x dx
                           y dy
                           anchor-y hand-anchor-y))
                 collect card-entity))))))
