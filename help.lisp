(in-package :lispstone)

(defun add-help-to-scene (scene)
  (with-slots (width height) scene
    (let ((dx 200)
          (dy 170))
      (add-to-scene scene (make-instance 'text-entity
                                         :id :help-dmg
                                         :x (- width dx)
                                         :y dy
                                         :font *small-font*
                                         :text (lambda ()
                                                 (i18n "ψ: damage"))))
      (add-to-scene scene (make-instance 'text-entity
                                         :id :help-dmg
                                         :x (- width dx)
                                         :y (+ dy *line-height*)
                                         :font *small-font*
                                         :text (lambda ()
                                                 (i18n "ρ: resist"))))
      (add-to-scene scene (make-instance 'text-entity
                                         :id :help-dmg
                                         :font *small-font*
                                         :x (- width dx)
                                         :y (+ dy (* 2 *line-height*))
                                         :text (lambda ()
                                                 (i18n "ξ: evasion")))))))
