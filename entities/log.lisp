(in-package :lispstone)

(defclass log-entity (entity)
  ((log :initarg :log)))

(defmethod render-entity ((entity log-entity) renderer)
  (with-fields (tex x y) entity
      (loop
         for i upto 10
         for dy = y then (+ dy *line-height*)
         for text in (log-records (slot-value entity 'log))
         do (with-text (renderer tex text :font *small-font*)
              (render-tex tex x dy)))))
