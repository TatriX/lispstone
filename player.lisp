(in-package :lispstone)

(defparameter *max-mana* 10)

(defclass player ()
  ((avatar
    :initform (make-instance 'avatar)
    :accessor player-avatar)
   (mana :accessor player-mana)
   (max-mana :accessor player-max-mana)
   (hand
    :initform (make-instance 'hand)
    :accessor player-hand)
   (deck :accessor player-deck)
   (name :accessor player-name
         :initarg :name
         :initform (error "Must supply a name"))))

(defun reset-player (player)
  (with-slots (hand deck mana max-mana avatar) player
    (reset-hand hand)
    (reset-avatar avatar)
    (setf mana 0
          max-mana 0
          deck (random-deck))))

(defmethod initialize-instance :after ((player player) &key)
  (reset-player player))

(defmethod print-object ((player player) stream)
  (with-slots (name mana deck hand) player
    (format stream "~a | mana ~2d | hand ~a | deck ~d"
            name mana (length hand) (length deck))))

(defun take-card (player)
  (with-slots (name hand deck) player
    (if deck
        (let ((card (pop deck)))
          (if (< (hand-size hand) *max-hand-size*)
              (progn
                (push-and-trace "~a ← ~a" name (card-name card))
                (add-to-hand hand card))
              (push-and-trace "Dropping ~a card: hand is full" (card-name card))))
        (push-and-trace "Deck is empty!"))))

(defun can-play-card (player card)
  (>= (player-mana player) (card-cost card)))


(defun remove-card (player card)
  (hand-remove-card (player-hand player) card))

(defun play-card (player card)
  (with-slots (mana name avatar) player
    (remove-card player card)
    (with-accessors ((card-name card-name) (cost card-cost)) card
      (push-and-trace "~&~a → ~a" name card-name)
      (decf mana cost)
      (funcall (card-effect card) avatar))))

(defun find-playable-card (player)
  (with-slots (hand) player
    (find-if (lambda (card) (can-play-card player card))
             (hand-shuffled-cards hand))))

(defun make-dmg (attacker resist)
  (round (* (avatar-dmg attacker) 0.01 (- 100 resist))))

(defun hit (attacker defending)
  (with-slots (hp evasion resist) defending
    (if (lucky evasion)
        'miss
        (let ((dmg (make-dmg attacker resist)))
          (decf hp dmg)
          dmg))))

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

(defun make-ai-turn (player)
  (log-trace "~&~%AI Turn of ~a~%" (player-name player))
  (loop
     for card = (find-playable-card player)
     while card
     do (play-card player card)))

(defun dead-p (player)
  (<= (avatar-hp (player-avatar player)) 0))
