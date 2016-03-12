(in-package :lispstone)

(defclass bonus-entity (entity)
  ((dx :initform (1+ (rounds-won)))
   (dy :initform (1+ (rounds-won)))
   (vx :initform (1+ (rounds-won)))
   (vy :initform (1+ (rounds-won)))
   (used :initform nil)
   (points :initarg points)))

(defmethod initialize-instance :before ((entity bonus-entity) &key &allow-other-keys)
  (with-slots (id points) entity
    (if (lucky 50)
        (setf id :good-bonus
              points 1)
        (setf id :bad-bonus
              points (- (+ 5 (random 9)))))))

(defmethod initialize-instance :after ((entity bonus-entity) &key &allow-other-keys)
  (with-slots (dx dy) entity
    (when (lucky 50)
      (setf dx (- dx)))
    (when (lucky 50)
      (setf dy (- dy)))))


(defmethod update-entity :before ((entity bonus-entity) scene)
  (with-slots (x y dx dy vx vy used) entity
    (unless used
        (with-slots (width height) scene
          (when (lucky 3)
            (setf vx (clamp (1+ vx) 0 *max-luck-scene-speed*)))
          (when (lucky 3)
            (setf vy (clamp (1+ vy) 0 *max-luck-scene-speed*)))

          (cond
            ((>= x (- width (entity-width entity)))
             (setf dx (- vx)))
            ((<= x 0)
             (setf dx vx)))

          (cond
            ((>= y (- height (entity-height entity)))
             (setf dy (- vy)))
            ((<= y 0)
             (setf dy vy)))

          (incf x dx)
          (incf y dy)))))

(defmethod entity-onclick ((entity bonus-entity))
  (with-slots (id used points) entity
    (unless used
      (setf used t)
      (entity-fade-out entity 10)
      (incf (player-bonuses *player*) points)
      (when (eq id :good-bonus)
        (incf *luck-score* points)))))
