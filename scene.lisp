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
    :accessor scene-graph)))

(defun add-to-scene (scene entity)
  (push entity (scene-graph scene)))

(defun remove-from-scene (scene id)
  (delete id (scene-graph scene) :key #'entity-id))

(defun find-entity (scene id)
  (find id (scene-graph scene) :key #'entity-id))

(defun find-entity-by (scene &key coords)
  (when coords
    (destructuring-bind (x y) coords
      (find-if (lambda (entity)
                 (%with-entity (tex ex ey ew eh) entity
                     (and (<= ex x)
                          (<= ey y)
                          (>= (+ ex ew) x)
                          (>= (+ ey eh) y)))) (scene-graph scene)))))


(defmethod initialize-instance :after ((scene scene) &key &allow-other-keys)
  (with-slots (renderer width height) scene
    (add-to-scene scene (make-instance 'player-entity
                                       :id :player-1
                                       :player *player-1*
                                       :y height
                                       :anchor-y :bottom))
    (add-to-scene scene (make-instance 'player-entity
                                       :id :player-2
                                       :player *player-2*
                                       :x width
                                       :y 0
                                       :anchor-x :right))
    (add-to-scene scene (make-instance 'hand-entity
                                       :id :hand
                                       :hand (player-hand *player-1*)
                                       :y (- height 10)
                                       :x (ash width -1)
                                       :anchor-y :bottom
                                       :anchor-x :center))
    (add-to-scene scene (make-instance 'avatar-entity
                                       :id :avatar-1
                                       :avatar (player-avatar *player-1*)
                                       :x (round (* 0.3 width))
                                       :y (ash height -1)
                                       :anchor-y :center))
    (add-to-scene scene (make-instance 'avatar-entity
                                       :id :avatar-2
                                       :avatar (player-avatar *player-2*)
                                       :x (round (* 0.6 width))
                                       :y (ash height -1)
                                       :anchor-y :center))
    (add-to-scene scene (make-instance 'log-entity
                                       :id :log
                                       :x 10
                                       :y 10
                                       :log *round-log*))
    (add-to-scene scene (make-instance 'entity
                                       :id :battlefield
                                       :x (ash width -1)
                                       :y (+ (ash height -1) 75) ;; FIXME:
                                       :anchor-x :center
                                       :anchor-y :center))
    (add-to-scene scene (make-instance 'entity
                                       :id :end-turn
                                       :x (- width 10)
                                       :y (- height 10)
                                       :anchor-x :right
                                       :anchor-y :bottom))
    (add-to-scene scene (make-instance 'entity
                                       :id :mute
                                       :x (- width 10)
                                       :y (ash height -1)
                                       :anchor-x :right
                                       :anchor-y :bottom))))

(defmacro with-scene-graph ((entity renderer) scene &body body)
  `(with-slots (graph ,renderer) ,scene
      (loop for ,entity in graph
         do (progn
              ,@body))))

(defun load-scene (scene)
  (with-scene-graph (entity renderer) scene
    (load-entity entity renderer)))

(defun update-scene (scene)
  (with-scene-graph (entity renderer) scene
    (update-entity entity)))

(defun render-scene (scene)
  (with-scene-graph (entity renderer) scene
    (render-entity entity renderer)
    (with-slots (width height) scene
      (let ((tex (load-texture-from-text renderer (format nil "~a (~a : ~a) ~a"
                                                          (player-name *player-1*)
                                                          (car *score*)
                                                          (cadr *score*)
                                                          (player-name *player-2*)))))
        (render-tex tex
                    (- (ash width -1) (ash (tex-width tex) -1))
                    10)))))
