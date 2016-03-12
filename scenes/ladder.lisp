(in-package :lispstone)

(defclass ladder-scene (scene)
  ())

(defparameter *enemy-icon-width* 80)
(defparameter *enemy-icon-height* 80)

(defparameter *current-enemy* 0)

(defmethod scene-onswitch ((scene ladder-scene))
  (clear-scene scene)
  (with-slots (renderer width height) scene
    (loop
       for enemy in (reverse *enemies*)
       for i = (length *enemies*) then (1- i)
       for y = 35 then (+ y *enemy-icon-height* 15)
       do (add-to-scene-and-load scene (make-instance 'entity
                                                      :id (make-keyword (format nil "ENEMY-ICON-~a" (player-name enemy)))
                                                      :x (ash width -1)
                                                      :y y
                                                      :anchor-x :center))
       when (<= i *current-enemy*) do
         (add-to-scene-and-load scene (make-instance 'entity
                                                     :id :done
                                                     :x (ash width -1)
                                                     :y y
                                                     :anchor-x :center))
       end)
    (add-to-scene-and-load scene (make-instance 'entity
                                                :id :mute
                                                :x (- width 10)
                                                :y (- (ash height -1) 135)
                                                :anchor-x :right))
    (let ((y (+ 35 (* (- (length *enemies*) *current-enemy* 1) (+ 15 *enemy-icon-height*)))))
      (add-to-scene-and-load scene (make-instance 'entity
                                                  :id :player-icon
                                                  :x (- (ash width -1) *enemy-icon-width* 40)
                                                  :y y
                                                  :anchor-x :center))
      (add-to-scene-and-load scene (make-instance 'entity
                                                  :id :start-round
                                                  :x (+ (ash width -1) 80)
                                                  :y (+ y 15)
                                                  :onclick (lambda ()
                                                             (set-state :round-start))))
      (add-to-scene-and-load scene (make-instance 'entity
                                                  :id :shop
                                                  :x (- (ash width -1) *enemy-icon-width* 100)
                                                  :y (+ y 60)
                                                  :anchor-x :right
                                                  :anchor-y :bottom
                                                  :onclick (lambda ()
                                                             (set-state :shop)))))))

;; (defmethod scene-onswitch ((scene ladder-scene))
;;     (with-slots (y) (find-entity scene :player-icon)
;;       (setf y dy))
;;     (with-slots (y) (find-entity scene :shop)
;;       (setf y (+ dy 60)))
;;     (with-slots (y) (find-entity scene :start-round)
;;       (setf y (+ dy 15)))))
