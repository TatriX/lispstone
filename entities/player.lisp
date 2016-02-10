(in-package :lispstone)

(defclass player-entity (entity)
  ((player :initarg :player)))

(defmethod entity-width ((entity player-entity))
  237)

(defmethod entity-height ((entity player-entity))
  101)

(defmethod render-entity :after ((entity player-entity) renderer)
  (with-slots (id x y player) entity
    (when (eq id :player-1)
      (with-slots (mana) player
        (let ((tex (load-texture-from-text renderer (format nil "(~a) ~v@{~A~:*~}" mana mana "λ"))))
          (render-tex tex
                      10
                      (- y (entity-height entity) *line-height*)))))))


      ;;   (let ((hp (avatar-hp (player-avatar *player-1*))))
      ;;   (render-hp renderer hp 0 (+ 4 *card-height*))
      ;;   (render-stats renderer *player-1*  (+ 4 *hp-width*) (+ 4 *card-height*)))
      ;; (let ((hp (avatar-hp (player-avatar *player-2*))))
      ;;   (render-hp renderer hp (- w *hp-width*) (+ 4 *card-height*))
      ;;   (render-stats renderer *player-2* (- w 400 *hp-width*) (+ 4 *card-height*)))

      ;; (let ((hand (player-hand *player-1*)))
      ;;   (render-hand renderer
      ;;                hand
      ;;                0
      ;;                0))
      ;; (let ((hand (player-hand *player-2*)))
      ;;   (render-hand renderer
      ;;                hand
      ;;                (- w (* (length hand) *card-width*))
      ;;                (- h *card-height*)))


      ;; (let ((tex (player-tex *player-2*)))
      ;;   (render-tex tex
      ;;               (- w (tex-width tex))
      ;;               (- (ash h -1) (ash (tex-height tex) -1))))
