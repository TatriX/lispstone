(in-package :lispstone)

(defclass avatar-entity (entity)
  ((avatar :initarg :avatar)))

(defparameter *hp-width* 120)
(defparameter *hp-height* *line-height*)

(defmethod render-entity :after ((entity avatar-entity) renderer)
  (%with-entity (tex x y w h) entity
    (with-slots (avatar) entity
      (render-hp renderer (avatar-hp avatar) x (- y *hp-height* 10))
      (render-avatar-stats renderer avatar x (- y *hp-height* 40)))))


(defun render-hp (renderer hp x y)
  (sdl2:set-render-draw-color renderer 122 22 22 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect x y *hp-width* *hp-height*))

  (sdl2:set-render-draw-color renderer 22 188 22 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect
                                   x
                                   y
                                   (round (* *hp-width* (/ hp *avatar-max-hp*)))
                                   *hp-height*))
  (let ((tex (load-texture-from-text renderer (format nil "Hp: ~a" hp))))
    (render-tex tex x y)))

(defun render-avatar-stats (renderer avatar x y)
  (let ((tex (load-texture-from-text renderer
                                     (with-slots (dmg resist evasion) avatar
                                       (format nil "| dmg: ~a | ρ: ~a | ξ: ~a |"
                                               dmg resist evasion)))))
    (render-tex tex x y)))
