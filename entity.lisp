(in-package :lispstone)

(defvar *entity-max-id* 0)

(defclass entity ()
  ((id
    :initarg :id
    :initform (incf *entity-max-id*)
    :reader entity-id)
   (visible-in
    :initarg :visible-in
    :initform nil
    :accessor entity-visible-in)
   (children
    :initform nil
    :accessor entity-children)
   (tex
    :initform nil
    :initarg :tex
    :accessor entity-tex)
   (tex-hover
    :initform nil
    :initarg :tex-hover
    :accessor entity-tex-hover)
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
    :writer (setf entity-x))
   (y
    :initarg :y
    :initform 0
    :writer (setf entity-y))
   (width
    :initarg :width
    :initform 0)
   (height
    :initarg :height
    :initform 0)
   (hover
    :initform nil
    :accessor entity-hover)
   (fadeout
    :initform 0)
   (fadealpha
    :initform 255)
   (onmouseover
    :initform nil
    :accessor entity-onmouseover)
   (onmouseout
    :initform nil
    :accessor entity-onmouseout)
   (onclick
    :initform nil
    :initarg :onclick
    :accessor entity-onclick)))

(defgeneric entity-x (entity))
(defgeneric entity-y (entity))
(defgeneric entity-width (entity))
(defgeneric entity-height (entity))

(defgeneric render-entity (entity renderer))
(defgeneric load-entity (entity renderer))
(defgeneric update-entity (entity scene))
(defgeneric entity-tex-name (entity))

(defgeneric entity-onmouseout (entity))
(defgeneric entity-onmouseover (entity))
(defgeneric entity-onclick (entity))

(defmethod entity-x ((entity entity))
  (with-slots (x anchor-x) entity
    (with-fields (width) entity
    (ecase anchor-x
      (:left x)
      (:center (- x (ash width -1)))
      (:right (- x width))))))

(defmethod entity-y ((entity entity))
  (with-slots (y anchor-y) entity
    (with-fields (height) entity
      (ecase anchor-y
        (:top y)
        (:center (- y (ash height -1)))
        (:bottom (- y height))))))

(defmethod entity-width ((entity entity))
  (with-slots (tex width) entity
    (if tex (tex-width tex) width)))

(defmethod entity-height ((entity entity))
  (with-slots (tex height) entity
    (if tex (tex-height tex) height)))

(defparameter *tex-cache* nil)

(defun init-tex-cache ()
  (setf *tex-cache* (make-hash-table :test #'equal)))

(defun load-entity-tex (renderer path)
  (multiple-value-bind (cached found) (gethash path *tex-cache*)
    (if found
        cached
        (let ((tex (when (probe-file path)
                     (load-texture-from-file renderer path))))
          ;; (when tex
          ;;     (log-trace "Loading texture ~a~%" path))
          (unless tex
              (log-trace "Texture ~a not found~%" path))
          (prog1
              tex
            (setf (gethash path *tex-cache*) tex))))))

(defmethod entity-tex-name ((entity entity))
  (format nil "~a" (entity-id entity)))

(defmethod entity-tex-path ((entity entity) &optional (suffix ""))
  (asset-path (string-downcase (format nil "~a~a.png" (entity-tex-name entity) suffix))))

(defmethod load-entity ((entity entity) renderer)
  (with-slots (tex tex-hover) entity
    (setf tex (load-entity-tex renderer (entity-tex-path entity))
          tex-hover (load-entity-tex renderer (entity-tex-path entity "-hover")))))

(defun entity-fade-out (entity fraction)
  (with-slots (fadeout fadealpha) entity
    (setf fadeout fraction)
    (setf fadealpha 255)))

(defmethod update-entity ((entity entity) scene)
  (with-slots (id fadeout fadealpha) entity
    (when (> fadealpha 0)
      (decf fadealpha fadeout)
      (when (<= fadealpha 0)
        (remove-from-scene scene entity)))))

(defmethod entity-onmouseout ((entity entity))
  (setf (entity-hover entity) nil))

(defmethod entity-onmouseover ((entity entity))
  (setf (entity-hover entity) t))

(defmethod entity-onclick ((entity entity))
  (with-slots (onclick) entity
    (when onclick
      (funcall onclick))))

(defun entity-clip (entity)
  (with-fields (tex width height frame last-frame-time) entity
      (when (and tex (> (tex-width tex) width))
        (prog1
            (sdl2:make-rect (* frame width)
                            0
                            width
                            height)
          (let ((now (get-internal-real-time)))
            (when (> (- now last-frame-time) 100)
              (setf last-frame-time now)
              (incf frame)
              (when (>= (* frame width) (tex-width tex))
                (setf frame 0))))))))


(defun entity-current-tex (entity)
  (with-slots (hover tex tex-hover) entity
    (if (and hover tex-hover) tex-hover tex)))

(defmethod render-entity ((entity entity) renderer)
  (with-fields (tex x y) entity
    (when tex
      (let ((clip (entity-clip entity)))
        (render-tex (entity-current-tex entity)
                    x y :clip clip)
        (when clip
          (sdl2:free-rect clip))))))


(defmethod render-entity :before ((entity entity) renderer)
  (with-slots (fadealpha) entity
    (let ((tex (entity-current-tex entity)))
      (when tex
        (with-slots (texture) tex
          (sdl2:set-texture-alpha-mod texture (truncate fadealpha)))))))


(defmethod render-entity :after ((entity entity) renderer)
  (with-slots (children) entity
    (loop for child in children
       do (render-entity child renderer))))
