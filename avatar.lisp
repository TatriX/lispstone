(in-package :lispstone)

(defclass avatar ()
  ((hp :initform *base-hp*
       :accessor avatar-hp
       :initarg :hp)
   (dmg :initform *base-dmg*
        :accessor avatar-dmg
        :initarg :dmg)
   (resist :initform 0
           :accessor avatar-resist
           :initarg :resist)
   (evasion :initform 0
            :accessor avatar-evasion
            :initarg :evasion)
   (name :initform nil
         :initarg :name)))

(defmethod print-object ((ava avatar) stream)
  (with-slots (hp dmg resist evasion) ava
    (format stream "hp ~3a | dmg ~2a | resist ~a | evasion ~a"
            hp dmg resist evasion)))


(defun update-avatar (avatar)
  (with-slots (dmg resist evasion) avatar
    (incf dmg)
    (setf resist (ash resist -1))
    (setf evasion (ash evasion -1))))

(defun make-avatar-turn (player)
  (with-accessors ((player-name player-name) (player-avatar player-avatar)) player
    (with-accessors ((enemy-name player-name) (enemy-avatar player-avatar)) (other-player player)
      (let ((result (hit player-avatar enemy-avatar)))
        (log-trace "~&~a: ~a" player-name player-avatar)
        (if (eq result 'miss)
            (push-and-trace "~a missed" player-name)
            (push-and-trace "~&~a → ~a → ~a " player-name result enemy-name))
        (log-trace "~&~a: ~a" enemy-name enemy-avatar)))))

(defparameter *avatar-max-hp* 100)

(defun heal (avatar value)
  (with-slots (hp) avatar
    (setf hp (min *avatar-max-hp* (+ hp value)))))


(defun reset-avatar (avatar)
  (with-slots (hp dmg resist evasion) avatar
      (setf hp *avatar-max-hp*
            dmg *base-dmg*
            resist 0
            evasion 0)))
