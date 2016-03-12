(in-package :lispstone)


(defparameter *game* nil)
(defparameter *player* (make-instance 'player :name "Lisp"))
(defparameter *enemy* (nth *current-enemy* *enemies*))

(defparameter *score* (list 0 0))
(defparameter *luck-score* 0)


(defun next-enemy ()
  (setf *enemy* (nth (incf *current-enemy*) *enemies*))
  (update-enemy-on-scene (find-scene *window* :table-scene)))

(defun update-enemy-on-scene (scene)
  (with-slots (width height) scene
    (remove-from-scene-by-id scene :enemy)
    (remove-from-scene-by-id scene :enemy-hand)
    (remove-from-scene-by-id scene :enemy-avatar)
    (add-to-scene-and-load scene (make-instance 'enemy-entity
                                       :id :enemy
                                       :player *enemy*
                                       :x width
                                       :y 0
                                       :anchor-x :right))
    (add-to-scene-and-load scene (make-instance 'hand-entity
                                       :id :enemy-hand
                                       :hand (player-hand *enemy*)
                                       :x (ash width -1)
                                       :y 10
                                       :anchor-x :center))
    (add-to-scene-and-load scene (make-instance 'avatar-entity
                                       :id :enemy-avatar
                                       :avatar (player-avatar *enemy*)
                                       :max-hp (avatar-hp (player-avatar *enemy*))
                                       :x *enemy-avatar-x*
                                       :y *enemy-avatar-y*
                                       :anchor-y :top))))

(defun rounds-won ()
  (car *score*))

(defun rounds-lost ()
  (cadr *score*))

(defun start-round ()
  (loop for player in (list *player* *enemy*)
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

(defun other-player (player)
  (if (equal player *player*)
      *enemy*
      *player*))

(defun next-turn ()
  (start-turn *player*)
  (make-turn *player*)
  (make-avatar-turn *player*)

  (start-turn *enemy*)
  (make-ai-turn *enemy*)
  (make-avatar-turn *enemy*)

  (log-trace "~&***~% ~a ~a" *player* *enemy*))


(defun buy-card (card)
  (with-slots (bought price) card
    (when (>= *luck-score* price)
        (setf bought t)
        (decf *luck-score* price))))
