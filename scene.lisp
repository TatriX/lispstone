(in-package :lispstone)

(defclass scene ()
  ((width
    :initarg :width
    :initform (error "Must supply a width"))
   (height
    :initarg :height
    :initform (error "Must supply a height"))
   (renderer
    :initarg :renderer
    :initform (error "Must supply a renderer"))
   (graph
    :initform '()
    :accessor scene-graph)
   (bg
    :initform nil)))

(defun scene-name (scene)
  (make-keyword (class-name (class-of scene))))

(defgeneric init-scene (scene))
(defgeneric update-scene (scene))
(defgeneric render-scene (scene))
(defgeneric scene-onswitch (scene))

(defmethod initialize-instance :after ((scene scene) &key &allow-other-keys)
  (init-scene scene)
  (load-scene scene))

(defun add-to-scene (scene entity)
  (with-slots (graph) scene
    (setf graph (nconc graph (list entity))))
  entity)

(defun add-to-scene-and-load (scene entity)
  (add-to-scene scene entity)
  (with-slots (renderer) scene
    (load-entity entity renderer))
  entity)

(defun remove-from-scene (scene entity)
  (with-slots (graph) scene
    (setf graph (remove entity graph :test #'equal))))

(defun remove-from-scene-by-id (scene id)
  (with-slots (graph) scene
    (setf graph (remove id graph :key #'entity-id))))

(defun find-entity (scene id)
  (find id (scene-graph scene) :key #'entity-id))

(defun find-entity-by (scene &key coords)
  (when coords
    (destructuring-bind (x y) coords
      (find-entity-by-coords x y (reverse (scene-graph scene))))))

(defun find-entity-by-coords (x y list)
  (loop
     for entity in list
     if (point-in-entity entity x y)
     do (return
          (let ((child (find-entity-by-coords x y (entity-children entity))))
            (if (and child (point-in-entity child x y))
                child
                entity)))))

(defun point-in-entity (entity mx my)
  (with-fields (x y width height) entity
    (and (<= x mx)
         (<= y my)
         (>= (+ x width) mx)
         (>= (+ y height) my))))

(defmacro with-scene-graph ((entity renderer) scene &body body)
  `(with-slots (graph ,renderer) ,scene
      (loop for ,entity in graph
         do (progn
              ,@body))))

(defun load-scene (scene)
  (with-scene-graph (entity renderer) scene
    (load-entity entity renderer)))

(defmethod scene-onswitch ((scene scene)))

(defmethod clear-scene ((scene scene))
  (with-slots (graph) scene
    (setf graph nil)))

(defmethod init-scene ((scene scene))
  (with-slots (bg renderer) scene
    (setf bg (load-texture-from-file renderer (asset-path (format nil "~a.png" (scene-name scene)))))))

(defmethod update-scene ((scene scene))
  (with-scene-graph (entity renderer) scene
    (update-entity entity scene)))

(defmethod render-scene ((scene scene))
  (render-tex (slot-value scene 'bg) 0 0)
  (with-scene-graph (entity renderer) scene
    (handler-case
        (with-slots (visible-in) entity
          (when (or (null visible-in) (find *state* visible-in))
            (render-entity entity renderer)))
      (error (err) (error (format nil "render error: ~a; entity ~a" err (entity-id entity)))))))
