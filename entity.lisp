(in-package :lispstone)

(defvar *entity-max-id* 0)

(defclass entity ()
  ((id
    :initarg :id
    :initform (incf *entity-max-id*)
    :reader entity-id)
   (tex
    :initform nil
    :accessor entity-tex)
   (tex-frame
    :initform 0
    :accessor entity-frame)
   (last-frame-time
    :initform 0
    :accessor entity-last-frame-time)
   (anchor-x
    :initarg :anchor-x
    :initform :left
    :accessor entity-anchor-x)
   (anchor-y
    :initarg :anchor-y
    :initform :top
    :accessor entity-anchor-y)
   (x
    :initarg :x
    :initform 0
    :accessor entity-x)
   (y
    :initarg :y
    :initform 0
    :accessor entity-y)
   (hover
    :initform nil)
   (onmouseover
    :initform nil
    :accessor entity-onmouseover)
   (onmouseout
    :initform nil
    :accessor entity-onmouseout)
   (onclick
    :initform nil
    :accessor entity-onclick)))

(defgeneric entity-width (entity))
(defgeneric entity-height (entity))
(defgeneric render-entity (entity renderer))
(defgeneric load-entity (entity renderer))
(defgeneric update-entity (entity))

(defmethod entity-width ((entity entity))
  (tex-width (entity-tex entity)))

(defmethod entity-height ((entity entity))
  (tex-height (entity-tex entity)))

(defmethod load-entity ((entity entity) renderer)
  (with-slots (tex id) entity
    (let ((path (asset-path (format nil"~a.png" id))))
      (when (probe-file path)
        (setf tex (load-texture-from-file renderer path))))))

(defmethod update-entity ((entity entity)))

(defmethod entity-onmouseout ((entity entity)))
(defmethod entity-onmouseover ((entity entity)))
(defmethod entity-onclick ((entity entity)))


(defmacro %with-entity ((tex x y width height) entity &body body)
  `(with-accessors ((,tex entity-tex)
                    (,x entity-x)
                    (,y entity-y)
                    (,width entity-width)
                    (,height entity-height)
                    (anchor-x entity-anchor-x)
                    (anchor-y entity-anchor-y)) ,entity
     (let ((,x (ecase anchor-x
                 (:left ,x)
                 (:center (- ,x (ash ,width -1)))
                 (:right (- ,x ,width))))
           (,y (ecase anchor-y
                 (:top ,y)
                 (:center (- ,y (ash ,height -1)))
                 (:bottom (- ,y ,height)))))
       ,@body)))

(defun entity-clip (entity)
  (with-accessors ((w entity-width)
                   (h entity-height)
                   (tex entity-tex)
                   (frame entity-frame)
                   (last entity-last-frame-time)) entity
    (with-slots (width height) tex
      (when (> width w)
        (prog1
            (sdl2:make-rect (* frame w)
                            0
                            w
                            h)
          (let ((now (get-internal-real-time)))
            (when (> (- now last) 100)
              (setf last now)
              (incf frame)
              (when (>= (* frame w) width)
                (setf frame 0)))))))))


(defmethod render-entity ((entity entity) renderer)
  (%with-entity (tex x y w h) entity
    (render-tex tex x y :clip (entity-clip entity))))
