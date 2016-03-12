(in-package :lispstone)

(defclass text-entity (entity)
  ((text :initarg :text)
   (color
    :initform nil
    :initarg :color)
   (font
    :initform *font*
    :initarg :font)))

(defmethod load-entity ((entity text-entity) renderer))

(defmethod render-entity ((entity text-entity) renderer)
  (with-slots (color font text width) entity
    (with-text (renderer tex (if (functionp text) (funcall text) text) :font font)
      (setf width (tex-width tex))
      (with-fields (x y) entity
        (render-tex tex x y)))))
