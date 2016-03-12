(in-package :lispstone)

(defclass table-scene (scene)
  ((delay :initform 0)
   (delay-callback :initform nil)))

(defparameter *player-avatar-x* 420)
(defparameter *player-avatar-y* 250)
(defparameter *enemy-avatar-x* 700)
(defparameter *enemy-avatar-y* 260)

(defmethod init-scene :after ((scene table-scene))
  (with-slots (renderer width height) scene
    (add-to-scene scene (make-instance 'entity
                                       :id :battlefield
                                       :x (ash width -1)
                                       :y (+ (ash height -1) 200)
                                       :anchor-x :center
                                       :anchor-y :bottom))
    (add-to-scene scene (make-instance 'player-entity
                                       :id :player
                                       :player *player*
                                       :y height
                                       :anchor-y :bottom))
    (add-to-scene scene (make-instance 'hand-entity
                                       :id :player-hand
                                       :hand (player-hand *player*)
                                       :x (ash width -1)
                                       :y (- height 10)
                                       :anchor-x :center
                                       :anchor-y :bottom))
    (add-to-scene scene (make-instance 'avatar-entity
                                       :id :player-avatar
                                       :avatar (player-avatar *player*)
                                       :x *player-avatar-x*
                                       :y *player-avatar-y*
                                       :anchor-y :top))
    (update-enemy-on-scene scene)
    (add-to-scene scene (make-instance 'log-entity
                                       :id :log
                                       :x 10
                                       :y 10
                                       :log *round-log*))
    (add-to-scene scene (make-instance 'entity
                                       :id :end-turn
                                       :visible-in '(:player)
                                       :x (ash width -1)
                                       :y (- (ash height -1) 50)
                                       :anchor-x :center
                                       :anchor-y :top))
    (add-to-scene scene (make-instance 'entity
                                       :id :mute
                                       :x (- width 10)
                                       :y (- (ash height -1) 135)
                                       :anchor-x :right))))

(defmethod update-scene :before ((scene table-scene))
  (with-slots (width height delay delay-callback) scene
    (let ((player-avatar (find-entity scene :player-avatar))
          (enemy-avatar (find-entity scene :enemy-avatar))
          (dx 10))
      (with-slots ((x1 x)) player-avatar
        (with-slots ((x2 x)) enemy-avatar
          (case *state*
            (:delay
             (when (<= (decf delay) 0)
               (funcall delay-callback)))
            (:player-attack-animation
             (if (<= x1 x2)
                 (progn
                   (incf x1 dx))
                 (progn
                   (setf x1 *player-avatar-x*)
                   (make-avatar-turn *player*)
                   (if (dead-p *enemy*)
                       (player-wins)
                       (progn
                         (setf delay 60
                               delay-callback (lambda ()
                                                (set-state :player-luck)))
                               (set-state :delay))))))
            (:enemy
             (setf delay 60
                   delay-callback (lambda ()
                                    (set-state :enemy-attack-animation)))
             (set-state :delay))
            (:enemy-attack-animation
             (if (>= x2 x1)
                 (progn
                   (decf x2 dx))
                 (progn
                   (setf x2 *enemy-avatar-x*)
                   (start-turn *enemy*)
                   (make-avatar-turn *enemy*)
                   (if (dead-p *player*)
                       (enemy-wins)
                       (progn
                         (setf delay 60
                               delay-callback (lambda ()
                                                (set-state :player)))
                         (set-state :delay))))))))))))
