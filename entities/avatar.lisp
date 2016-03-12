(in-package :lispstone)

(defclass avatar-entity (entity)
  ((avatar :initarg :avatar)
   (max-hp :initarg :max-hp
           :initform *avatar-max-hp*)))

(defparameter *hp-width* 120)
(defparameter *hp-height* *line-height*)

(defmethod render-entity :after ((entity avatar-entity) renderer)
  (with-fields (tex x y) entity
    (with-slots (avatar max-hp) entity
      (render-hp renderer (avatar-hp avatar) max-hp x (- y *hp-height* 10))
      (render-avatar-stats renderer avatar x (- y *hp-height* 40)))))


(defun render-hp (renderer hp max-hp x y)
  (sdl2:set-render-draw-color renderer 167 9 9 255)
  (let ((hp-rect (sdl2:make-rect x y *hp-width* *hp-height*))
        (fill-rect (sdl2:make-rect x y (round (* *hp-width* (/ hp max-hp))) *hp-height*)))
    (sdl2:render-fill-rect renderer hp-rect)
    (sdl2:set-render-draw-color renderer 11 128 22 255)
    (sdl2:render-fill-rect renderer fill-rect)
    (sdl2:free-rect hp-rect)
    (sdl2:free-rect fill-rect))
  (with-text (renderer tex (format nil "HP: ~a" hp))
    (render-tex tex x y)))

(defun render-avatar-stats (renderer avatar x y)
  (with-text (renderer tex (with-slots (dmg resist evasion) avatar
                             (format nil "ψ: ~a | ρ: ~a | ξ: ~a"
                                     dmg resist evasion)))
    (render-tex tex
                (- x 27)
                (- y 5))))

(defmethod entity-tex-name ((entity avatar-entity))
  (with-slots (avatar) entity
    (with-slots (name) avatar
      (if name
          (format nil  "avatar-~a" name)
          (call-next-method)))))
