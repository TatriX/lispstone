(in-package :lispstone)

(defclass enemy-entity (player-entity)
  ())


(defmethod render-entity :after ((entity enemy-entity) renderer)
  (with-slots (x y player) entity
    (render-mana renderer (player-mana player) (- x 200) (+ y 100))
    (render-score renderer (rounds-lost) (- x 185) (+ y 23))))


(defmethod entity-tex-name ((entity enemy-entity))
  (with-slots (player) entity
    (format nil  "enemy-~a" (player-name player))))
