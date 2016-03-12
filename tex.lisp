(in-package :lispstone)

(defclass tex ()
  ((renderer
    :initarg :renderer
    :initform (error "Must supply a renderer")
    :accessor tex-renderer)
   (width
    :accessor tex-width
    :initform 0 )
   (height
    :accessor tex-height
    :initform 0)
   (texture
    :accessor tex-texture
    :initform nil)))

(defun load-texture-from-texture (renderer sdl-texture)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (setf width (sdl2:texture-width sdl-texture))
      (setf height (sdl2:texture-height sdl-texture))
      (setf texture sdl-texture))
    tex))

(defun load-texture-from-file (renderer filename)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-image:load-image filename)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                        0 #xFF #xFF))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

(defun load-texture-from-text (renderer text &key (font *font*) (r 227) (g 215) (b 186) (a 255))
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-ttf:render-utf8-blended font text r g b a)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (setf texture (sdl2:create-texture-from-surface renderer surface))
        (sdl2:free-surface surface)))
    tex))

(defun set-color (tex r g b)
  (sdl2:set-texture-color-mod (tex-texture tex) r g b))

(defun render-tex (tex x y &key clip angle center flip)
  (with-slots (renderer texture width height) tex
    (let ((rect (sdl2:make-rect x
                                y
                                (if clip (sdl2:rect-width clip) width)
                                (if clip (sdl2:rect-height clip) height))))
      (sdl2:render-copy-ex renderer
                           texture
                           :source-rect clip
                           :dest-rect rect
                           :angle angle
                           :center center
                           :flip (list (or flip :none)))
      (sdl2:free-rect rect))))

(defun destroy-tex (tex)
  (with-slots (texture) tex
    (sdl2:destroy-texture texture)))

(defmacro with-text ((renderer tex text &rest rest) &body body)
  `(let ((,tex (load-texture-from-text ,renderer ,text ,@rest)))
     ,@body
     (destroy-tex ,tex)))
