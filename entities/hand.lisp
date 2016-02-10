(in-package :lispstone)

(defclass hand-entity (entity)
  ((hand :initarg :hand)))

(defmethod entity-width ((entity hand-entity))
  (with-slots (hand) entity
    (* (hand-size hand) *card-width*)))

(defmethod entity-height ((entity hand-entity))
  *card-height*)

(defmethod render-entity ((entity hand-entity) renderer)
  (%with-entity (tex x y w h) entity
    (with-slots (hand) entity
      (loop
         for card in (hand-cards hand)
         for dx = x then (+ dx *card-width*)
         for i = 0 then (1+ i)
         do (%render-card card dx y i)))))

(defun %render-card (card x y index)
  (funcall (if (= index *hovered-card*)
               #'render-card-hover
               #'render-card)
           card x y))
