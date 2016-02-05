(in-package :twg)

(defclass card ()
  ((name :initarg :name
         :accessor card-name)
   (cost :initarg :cost
         :accessor card-cost)
   (desc :initarg :desc
         :accessor card-desc)
   (effect :initarg :effect
           :accessor card-effect
           :initform (error "Must supply an effect"))))

(defmethod print-object ((card card) stream)
  (with-slots (name cost desc) card
    (format stream "~a | cost ~a | ~a" name cost desc)))

(defparameter *cards* (list (make-instance 'card
                                           :name "()"
                                           :cost 1
                                           :desc "+5 hp"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-hp avatar) 5)))
                            (make-instance 'card
                                           :name "[]"
                                           :cost 2
                                           :desc "+2 dmg"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-dmg avatar) 2)))
                            (make-instance 'card
                                           :name "{}"
                                           :cost 3
                                           :desc "+10% ρ"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-resist avatar) 10)))
                            (make-instance 'card
                                           :name "<>"
                                           :cost 4
                                           :desc "+10% ξ"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-evasion avatar) 5)))
                            (make-instance 'card
                                           :name "⊂⊃"
                                           :cost 5
                                           :desc "+50 hp"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-hp avatar) 50)))
                            (make-instance 'card
                                           :name "≺≻"
                                           :cost 6
                                           :desc "+20 dmg"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-dmg avatar) 20)))
                            (make-instance 'card
                                           :name "⊏⊐"
                                           :cost 7
                                           :desc "+50% ρ,ξ"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-resist avatar) 50)
                                                     (incf (avatar-evasion avatar) 50)))))

(defun find-card-by-name (name &optional (cards *cards*))
  (find name cards :key #'card-name :test 'equal))

(defparameter *base-hp* 100)
(defparameter *base-dmg* 10)

(defparameter *game-log* nil)
(defparameter *game-log-max-size* 15)
(defparameter *trace-enabled* t)

(defun push-to-log (fmt &rest args)
  (push (apply #'format nil fmt args) *game-log*)
  (when (> (length *game-log*) *game-log-max-size*)
    (pop *game-log*)))

(defun log-trace (fmt &rest args)
  (when *trace-enabled*
    (apply #'format *standard-output* fmt args)))

(defun push-and-trace (fmt &rest args)
  (apply #'push-to-log fmt args)
  (apply #'log-trace fmt args))

(defclass avatar ()
  ((hp :initform *base-hp*
       :accessor avatar-hp)
   (dmg :initform *base-dmg*
        :accessor avatar-dmg)
   (resist :initform 0
           :accessor avatar-resist)
   (evasion :initform 0
            :accessor avatar-evasion)))

(defmethod print-object ((ava avatar) stream)
  (with-slots (hp dmg resist evasion) ava
    (format stream "hp ~3a | dmg ~2a | resist ~a | evasion ~a"
            hp dmg resist evasion)))

(defclass player ()
  ((avatar :accessor player-avatar)
   (mana :accessor player-mana)
   (max-mana :accessor player-max-mana)
   (hand :accessor player-hand)
   (deck :accessor player-deck)
   (name :accessor player-name
         :initarg :name
         :initform (error "Must supply a name"))
   (tex :accessor player-tex)))

(defun random-deck ()
  (loop for i below *deck-size*
     for prototype = (random-elt *cards*)
     collect (make-instance 'card
                            :name (card-name prototype)
                            :cost (card-cost prototype)
                            :desc (card-desc prototype)
                            :effect (card-effect prototype))))

(defun reset-player (player)
  (with-slots (hand deck mana max-mana avatar) player
    (setf mana 0
          max-mana 0
          hand '()
          deck (random-deck)
          avatar (make-instance 'avatar))))

(defmethod initialize-instance :after ((player player) &key)
  (reset-player player))

(defmethod print-object ((player player) stream)
  (with-slots (name mana deck hand) player
    (format stream "~a | mana ~2d | hand ~a | deck ~d"
            name mana (length hand) (length deck))))

(defparameter *deck-size* 30)
(defparameter *max-hand-size* 7)

(defparameter *game* nil)
(defparameter *player-1* (make-instance 'player :name "Абу"))
(defparameter *player-2* (make-instance 'player :name "Виталик"))
(defparameter *players* (list *player-1* *player-2*))
(defparameter *max-mana* 10)

(defun take-card (player)
  (with-slots (name hand deck) player
    (if deck
        (let ((card (pop deck)))
          (if (< (length hand) *max-hand-size*)
              (progn
                (push-and-trace "~a ← ~a" name (card-name card))
                (push card hand))
              (push-and-trace "Dropping ~a card: hand is full" (card-name card))))
        (push-and-trace "Deck is empty!"))))

(defun start-round ()
  (loop for player in *players*
     do
       (reset-player player)
       (loop for i below 3
          do (take-card player)))
    (setf *game-log* nil))

(defun start-turn (player)
  (with-slots (mana max-mana hand deck avatar) player
    (when (< max-mana *max-mana*)
      (incf max-mana))
    (setf mana max-mana)
    (take-card player)
    (update-avatar avatar)))

(defun prompt-read (prompt)
  (format *query-io* "~&~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun find-card-by-name-or-index (player name-or-index)
  (handler-case
      (let ((index (parse-integer name-or-index)))
        (nth index (player-hand player)))
    (error () (find-card-by-name name-or-index))))

(defun format-hand (hand)
  (format nil "~{~%~a~}"
          (loop
             for card in hand
             for i = 0 then (1+ i)
             collect (format nil "~a | ~a" i card))))

(defun make-turn (player)
  (with-slots (name mana avatar hand) player
    (log-trace "~&Turn of ~a~%" name)
    (log-trace "Mana ~a~%" mana)
    (log-trace "Avatar ~a~%" avatar)
    (log-trace "Hand: ~a~%" (format-hand hand)))
  (loop
     for input = (prompt-read "Play card (index | name | end)")
     until (equalp input "end")
     for card = (find-card-by-name-or-index player input)
     do (if card
            (play-card player card)
            (log-trace "No card to play"))))

(defun other-player (player &optional (players *players*))
  (if (equal player (car players))
      (cadr players)
      (car players)))

(defun can-play-card (player card)
  (>= (player-mana player) (card-cost card)))

(defun remove-card (player card)
  (delete card (player-hand player)))

(defun play-card (player card)
  (with-slots (mana name avatar) player
    (remove-card player card)
    (with-accessors ((card-name card-name) (cost card-cost)) card
      (push-and-trace "~&~a → ~a" name card-name)
      (decf mana cost)
      (funcall (card-effect card) avatar))))


(defun roll-dice ()
  (random 100))

(defun lucky (value)
  (< (roll-dice) value))

(defun make-dmg (attacker resist)
  (round (* (avatar-dmg attacker) 0.01 (- 100 resist))))

(defun hit (attacker defending)
  (with-slots (hp evasion resist) defending
    (if (lucky evasion)
        'miss
        (let ((dmg (make-dmg attacker resist)))
          (decf hp dmg)
          dmg))))

(defun update-avatar (avatar)
  (with-slots (dmg resist evasion) avatar
    (when (> dmg *base-dmg*)
      (decf dmg))
    (setf resist (ash resist -1))
    (setf evasion (ash evasion -1))))

(defun make-avatar-turn (player)
  (with-accessors ((player-name player-name) (player-avatar player-avatar)) player
    (with-accessors ((enemy-name player-name) (enemy-avatar player-avatar)) (other-player player)
      (let ((result (hit player-avatar enemy-avatar)))
        (log-trace "~&~a: ~a" player-name player-avatar)
        (if (eq result 'miss)
            (push-and-trace "~&~a's avatar missed" player-name)
            (push-and-trace "~&~a → ~a → ~a " player-name result enemy-name))
        (log-trace "~&~a: ~a" enemy-name enemy-avatar)))))

(defun find-playable-card (player)
  (let ((hand (player-hand player)))
    (when hand
      (find-if (lambda (card) (can-play-card player card))
               (shuffle hand)))))

(defun make-ai-turn (player)
  (log-trace "~&~%AI Turn of ~a~%" (player-name player))
  (loop
     for card = (find-playable-card player)
     while card
     do (play-card player card)))

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
