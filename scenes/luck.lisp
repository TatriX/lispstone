(in-package :lispstone)

(defclass luck-scene (scene)
  ((last-update :initform 0)
   (timer :initform 0)))

(defmethod scene-onswitch ((scene luck-scene))
  (with-slots (last-update timer renderer width height) scene
    (setf last-update (get-internal-real-time)
          timer 10
          (player-bonuses *player*) (- (random 4)))

    (clear-scene scene)
    (add-to-scene scene (make-instance 'text-entity
                                       :id :luck-timer
                                       :x (ash width -1)
                                       :y 10
                                       :anchor-x :center
                                       :text (lambda ()
                                               (with-slots (time) scene
                                                 (tt "Time left: ~2d | Luck: ~2d"
                                                         timer (player-bonuses *player*))))))
    (loop repeat (+ 10 (rounds-won))
       do (add-to-scene-and-load scene (make-instance 'bonus-entity
                                                      :x (random (- width *card-width*))
                                                      :y (random (- height *card-height*)))))))


(defmethod update-scene :before ((scene luck-scene))
  (with-slots (timer last-update graph) scene
    (if (or (zerop timer) (zerop (count :good-bonus graph :key #'entity-id)))
        (progn
          (consume-bonuses *player*)
          (set-state :player-attack-animation))
        (let ((now (get-internal-real-time)))
          (when (> (- now last-update) 1000)
            (decf timer)
            (setf last-update now))))))
