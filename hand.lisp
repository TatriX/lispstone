(in-package :lispstone)

(defparameter *max-hand-size* 7)

(defclass hand ()
  ((cards :initform '()
          :accessor hand-cards)))

(defun hand-size (hand)
  (length (hand-cards hand)))

(defun add-to-hand (hand card)
  (with-slots (cards) hand
    (push card cards)))

(defun hand-shuffled-cards (hand)
  (shuffle (hand-cards hand)))

(defun format-hand (hand)
  (format nil "~{~%~a~}"
          (loop
             for card in (hand-cards hand)
             for i = 0 then (1+ i)
             collect (format nil "~a | ~a" i card))))

(defun hand-remove-card (hand card)
  (with-slots (cards) hand
    (setf cards (remove card cards))))

(defun reset-hand (hand)
  (setf (hand-cards hand) nil))
