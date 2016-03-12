(in-package :lispstone)

(defclass player ()
  ((avatar
    :initarg :avatar
    :initform (make-instance 'avatar)
    :accessor player-avatar)
   (avatar-proto :initarg :avatar-proto)
   (mana :accessor player-mana)
   (max-mana :accessor player-max-mana)
   (hand
    :initform (make-instance 'hand)
    :accessor player-hand)
   (deck :accessor player-deck)
   (name :accessor player-name
         :initarg :name
         :initform (error "Must supply a name"))
   (bonuses
    :initform 0
    :accessor player-bonuses)
   (luck :initform nil)))

(defun reset-player (player)
  (with-slots (hand deck mana max-mana avatar avatar-proto) player
    (reset-hand hand)
    (if (slot-boundp player 'avatar-proto)
        (with-slots (name hp dmg resist evasion) avatar
          (with-slots ((name-x name)
                       (hp-x hp)
                       (dmg-x dmg)
                       (resist-x resist)
                       (evasion-x evasion)) avatar-proto
            (setf name name-x
                  hp hp-x
                  dmg dmg-x
                  resist resist-x
                  evasion evasion-x)))
        (reset-avatar avatar))
    (setf mana 0
          max-mana 0
          deck (random-deck (ecase (type-of player)
                              (player (available-cards))
                              (enemy *cards*))))))

(defmethod initialize-instance :after ((player player) &key)
  (reset-player player))

(defmethod print-object ((player player) stream)
  (with-slots (name mana deck hand) player
    (format stream "~a | mana ~2d | hand ~a | deck ~d"
            name mana (hand-size hand) (length deck))))

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
    (apply-card card player))
  (let ((clone (clone-card-entity (get-card-entity card) card))
        (scene (find-scene *window* :table-scene)))
    (with-slots (width height) scene
      (with-slots (id x y anchor-x anchor-y) clone
        (setf id :played-card
              x (ash width -1)
              y (+ (ash height -1) 50)
              anchor-x :center
              anchor-y :center)))
    (entity-fade-out clone 1.5)
    (add-to-scene scene clone)))

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

(defun consume-bonuses (player)
  (with-slots (luck bonuses) player
    (setf luck (plusp bonuses)
          bonuses 0)))
