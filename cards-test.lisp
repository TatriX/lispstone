(in-package :lispstone)

(use-package :prove)

(plan 3)

(let ((player (make-instance 'player :name "Tester")))
  (reset-player player)
  (is (hand-size (player-hand player)) 0)
  (start-round (list player))
  (is (hand-size (player-hand player)) *start-cards-num*)
  (setf (player-mana player) *max-mana*)
  (let ((card (find-playable-card player)))
    (play-card player card)
    (is (hand-size (player-hand player)) (- *start-cards-num* 1))))

(finalize)
