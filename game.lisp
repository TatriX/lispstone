(in-package :lispstone)

(defparameter *base-hp* 100)
(defparameter *base-dmg* 10)

(defparameter *game* nil)
(defparameter *player-1* (make-instance 'player :name "Абу"))
(defparameter *player-2* (make-instance 'player :name "Виталик"))
(defparameter *players* (list *player-1* *player-2*))
(defparameter *start-cards-num* 1)

(defparameter *score* (list 0 0))

(defun start-round ()
  (loop for player in *players*
     do
       (reset-player player)
       (loop for i below *start-cards-num*
          do (take-card player)))
  (clear-log *round-log*))

(defun start-turn (player)
  (with-slots (mana max-mana hand deck avatar) player
    (when (< max-mana *max-mana*)
      (incf max-mana))
    (setf mana max-mana)
    (take-card player)
    (update-avatar avatar)))

(defun other-player (player &optional (players *players*))
  (if (equal player (car players))
      (cadr players)
      (car players)))

(defun next-turn ()
  (start-turn *player-1*)
  (make-turn *player-1*)
  (make-avatar-turn *player-1*)

  (start-turn *player-2*)
  (make-ai-turn *player-2*)
  (make-avatar-turn *player-2*)

  (log-trace "~&***~% ~{~a~%~^~}" *players*))

(defun test()
  (dotimes (i 30) (next-turn)))
