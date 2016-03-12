(in-package :lispstone)


(defclass shop-scene (scene)
  ())

(defmethod scene-onswitch ((scene shop-scene))
  (clear-scene scene)
  (with-slots (width height) scene
    (loop for card in *cards*
       for card-entity = (clone-card-entity (get-card-entity card) card)
       for dx = 70 then (+ dx *card-height* 10)
       for cost = 10 then (+ cost 10)
       with dy = 50
       when (> dx 900) do
         (setf dx 70)
         (incf dy (+ 30 *card-height*))
       end
       when (= dy 50) do
         (add-to-scene-and-load scene (make-instance 'text-entity
                                                     :x (+ dx (ash *card-width* -1))
                                                     :y 10
                                                     :anchor-x :center
                                                     :text (tt "~d" cost)))
       end
       do (with-slots (x y) card-entity
            (setf x dx
                  y dy)
            (add-to-scene scene card-entity)))
    (add-to-scene-and-load scene (make-instance 'text-entity
                                                :x 10
                                                :y 10
                                                :text (tt "Cost:")))
    (add-to-scene-and-load scene (make-instance 'text-entity
                                                :x (- width 10)
                                                :y 10
                                                :anchor-x :right
                                                :text (lambda ()
                                                        (tt "Luck: ~a" *luck-score*))))
    (add-to-scene-and-load scene (make-instance 'entity
                                                :id :back
                                                :x (- width 10)
                                                :y (- height 10)
                                                :anchor-x :right
                                                :anchor-y :bottom
                                                :onclick (lambda ()
                                                           (set-state :ladder))))
    (add-help-to-scene scene)
    (add-to-scene-and-load scene (make-instance 'text-entity
                                                :x (- width 200)
                                                :y 280
                                                :font *small-font*
                                                :text (i18n "Here you can")))
    (add-to-scene-and-load scene (make-instance 'text-entity
                                                :x (- width 200)
                                                :y (+ 280 *line-height*)
                                                :font *small-font*
                                                :text (i18n "ban cards")))))
