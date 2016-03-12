(in-package :lispstone)

(defclass card ()
  ((id
    :initarg :id
    :reader card-id)
   (name :initarg :name
         :accessor card-name)
   (cost :initarg :cost
         :accessor card-cost)

   (buff-desc :initarg :buff-desc)
   (buff :initarg :buff)

   (debuff-desc :initarg :debuff-desc)
   (debuff :initarg :debuff)
   (price :initform 0)
   (bought
    :initarg :bought
    :accessor card-bought)))

(defmethod print-object ((card card) stream)
  (with-slots (name cost buff-desc debuff-desc) card
    (format stream "~a | cost ~a | ~a | ~a" name cost buff-desc debuff-desc)))

(defun find-card-by-name (name &optional (cards *cards*))
  (find name cards :key #'card-name :test 'equal))

(defun find-card-by-name-or-index (player name-or-index)
  (handler-case
      (let ((index (parse-integer name-or-index)))
        (nth index (player-hand player)))
    (error () (find-card-by-name name-or-index))))

(defun card-find-name (card index)
  (with-slots (name) card
    (cdr (find-if (lambda (c) (equal (char (car c) index) (char name index))) *card-file-names*))))

(defun card-top-name (card)
  (with-slots (buff) card
    (concatenate 'string (card-find-name card 0) (if buff "b" "r"))))

(defun card-bottom-name (card)
  (with-slots (debuff) card
    (concatenate 'string (card-find-name card 1) (if debuff "r" "b"))))

(defun apply-card (card player)
  (with-slots (name cost buff debuff) card
    (with-slots ((player-name name) mana avatar) player
      (push-and-trace "~&~a → ~a" player-name name)
      (decf mana cost)
      (when buff
        (funcall buff avatar))
      (when debuff
        (funcall debuff (player-avatar (other-player player)))))))
