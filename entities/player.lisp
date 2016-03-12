(in-package :lispstone)

(defclass player-entity (entity)
  ((player :initarg :player)))

(defmethod render-entity :after ((entity player-entity) renderer)
  (with-slots (x y player) entity
    (render-total-luck renderer 10 (- y 180))
    (render-score renderer (rounds-won) (+ x 173) (- y 45))
    (render-mana renderer
                 (player-mana player)
                 (- (entity-width entity) 70)
                 (+ (- y (entity-height entity)) 40))))

(defun render-total-luck (renderer x y)
  (with-text (renderer tex (tt "Luck: ~a" *luck-score*))
    (render-tex tex x y)))

(defun render-score (renderer score x y)
  (with-text (renderer tex (format nil "~a" score))
    (render-tex tex x y)))

(defun render-mana (renderer mana x y)
  (let ((r 234)
        (g 220)
        (b 188))
    (with-text (renderer tex (format nil "~aλ" mana) :r r :g g :b b)
      (render-tex tex x y))))
