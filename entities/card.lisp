(in-package :lispstone)

(defclass card-entity (entity)
  ((card :initarg :card
         :accessor entity-card)))

(defun load-card-texture (renderer card path)
  (with-slots (name cost buff buff-desc debuff-desc) card
    (let ((r 234)
          (g 220)
          (b 188)
          (padding-top 35))
      (let ((bg-tex (load-texture-from-file renderer (asset-path path)))
            (name-tex-1 (load-texture-from-file renderer (asset-path (format nil "~a.png" (card-top-name card)))))
            (name-tex-2 (load-texture-from-file renderer (asset-path (format nil "~a.png" (card-bottom-name card)))))
            (cost-tex (load-texture-from-text renderer (format nil "~aλ" cost) :r r :g g :b b))
            (tex (sdl2:create-texture renderer
                                      *default-pixel-format*
                                      :target
                                      *card-width*
                                      *card-height*)))
        (sdl2:set-texture-blend-mode tex :blend)
        (sdl2:set-render-target renderer tex)
        (render-tex bg-tex 0 0)
        (render-tex name-tex-1 10 10)
        (render-tex name-tex-2
                    (- (tex-width bg-tex) (tex-width name-tex-1) 10)
                    (- (tex-height bg-tex) (tex-height name-tex-1) 10)
                    :flip :horizontal)
        (render-tex cost-tex
                    (- (tex-width bg-tex) (tex-width cost-tex) 15)
                    10)
        (unless (emptyp buff-desc)
          (let ((buff-desc-tex (load-texture-from-text renderer
                                                       buff-desc
                                                       :font *small-font*
                                                       :r 11 :g 128 :b 22)))
            (render-tex buff-desc-tex
                        15
                        (+ padding-top *small-line-height*))))
        (unless (emptyp debuff-desc)
          (let ((debuff-desc-tex (load-texture-from-text renderer
                                                         debuff-desc
                                                         :font *small-font*
                                                         :r 167 :g 9 :b 9 )))

            (render-tex debuff-desc-tex
                        15
                        (+ padding-top (* (if buff 2 1) *small-line-height*)))))
        (sdl2:set-render-target renderer nil)
        (load-texture-from-texture renderer tex)))))

(defmethod entity-onclick ((entity card-entity))
  (with-slots (card) entity
    (cond
      ((member card (hand-cards (player-hand *player*)))
       (if (can-play-card *player* card)
           (play-card *player* card)
           (push-and-trace (tt "Not enough λ to play ~a" (card-name card)))))
      ((and (slot-boundp card 'bought) (not (card-bought card)))
       (buy-card card)))))

(defmethod entity-onmouseover ((entity card-entity))
  (unless (eq (entity-id entity) :played-card)
    (call-next-method)))

(defparameter *card-entities* nil)

(defparameter *card-back-entity* nil)

(defun load-card-entities (renderer &optional (cards *cards*))
  (setf *card-entities* (make-hash-table :test #'equal))
  (setf *card-back-entity* (make-instance 'entity :id :card-back))
  (load-entity *card-back-entity* renderer)
  (loop for card in cards
     for id = (card-id card)
     for card-entity = (make-instance 'card-entity
                                      :id (format nil "card-~a" id)
                                      :tex (load-card-texture renderer card "card-face.png")
                                      :tex-hover (load-card-texture renderer card "card-face-hover.png"))
     do
       (setf (gethash id *card-entities*) card-entity)))

(defun get-card-entity (card)
  (gethash (card-id card) *card-entities*))

(defun clone-card-entity (entity card)
  (with-slots (id tex tex-hover) entity
      (make-instance (class-of entity)
                     :id id
                     :card card
                     :tex tex
                     :tex-hover tex-hover)))

(defun make-card-back-entity ()
  (with-slots (id tex tex-hover) *card-back-entity*
      (make-instance 'entity
                     :id :card-back
                     :tex tex
                     :tex-hover tex-hover)))

(defmethod render-entity :after ((entity card-entity) renderer)
  (with-fields (x y width height card) entity
    (when (slot-boundp card 'bought)
      (when (card-bought card)
        (let ((rect (sdl2:make-rect x y width height)))
          (sdl2:set-render-draw-color renderer 0 0 0 100)
          (sdl2:set-render-draw-blend-mode renderer :blend)
          (sdl2:render-fill-rect renderer rect)
          (sdl2:set-render-draw-blend-mode renderer :none)
          (sdl2:free-rect rect))))))
