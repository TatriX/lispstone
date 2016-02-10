(in-package :lispstone)

(defclass log-entity (entity)
  ((log :initarg :log)))

(defmethod render-entity ((entity log-entity) renderer)
  (%with-entity (tex x y w h) entity
    (with-slots (log) entity
      (loop
         for i upto 10
         for dy = y then (+ dy *line-height*)
         for text in (log-records log)
         ;; TODO: cache
         do (let ((tex (load-texture-from-text renderer text)))
              (render-tex tex x dy))))))

(defmethod entity-width ((entity log-entity))
  0)

(defmethod entity-height ((entity log-entity))
  0)
