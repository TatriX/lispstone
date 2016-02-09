;;; HOW TO USE:
;;;
;;; First, run this.  It is SAFE to run repeatedly:
;;;
;;;   (kit.sdl2:start)
;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'kit.sdl2.test:simple-window)
;;;
;;; You can make multiple windows if you want.  Note that, despite not
;;; assigning the value, THIS IS NOT COLLECTED.  A reference is kept
;;; to all windows:
;;;
;;;   (all-windows)
;;;
;;; After you close a window, it will be collected at some point.
;;;
;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).

;; (setf sb-impl::*default-external-format* :UTF-8)

(in-package :twg)

(defparameter *window* nil)
(defparameter *default-pixel-format* nil)
(defparameter *debug-mode* nil)

(defclass simple-window (test-window)
  ((renderer :initform nil
             :accessor window-renderer)))

;;; All of these methods are OPTIONAL.  However, without a render
;;; method, your window will not look like much!


;;; Note this is an :AFTER method.  You should either use :AFTER, or
;;; you must (CALL-NEXT-METHOD).

(defun asset-path (name)
  (merge-pathnames
   (concatenate 'string "assets/" name) (asdf/system:system-source-directory :twg)))

(defparameter *font-name* (asset-path "DejaVuSans.ttf"))

(defparameter *font* nil)
(defparameter *font-size* 28)

(defparameter *small-font* nil)
(defparameter *small-font-size* 14)

(defparameter *line-height* (round (* 1.3 *font-size* )))

(defparameter *card-textures* nil)

(defun load-card-texture (renderer card)
  (with-slots (name cost desc) card
    (let ((bg-tex (load-texture-from-file renderer (asset-path "card.png")))
          (name-tex (load-texture-from-text renderer name))
          (cost-tex (load-texture-from-text renderer (format nil "λ: ~a" cost) :font *small-font*))
          (desc-tex (load-texture-from-text renderer desc :font *small-font*))
          (tex (sdl2:create-texture renderer
                                    *default-pixel-format*
                                    :target
                                    *card-width*
                                    *card-height*)))
      (sdl2:set-render-target renderer tex)
      (render-tex bg-tex 0 0)
      (render-tex name-tex 10 0)
      (render-tex cost-tex 10 (* 1 *line-height*))
      (render-tex desc-tex 10 (* 2 *line-height*))
      (sdl2:set-render-target renderer nil)
      (load-texture-from-texture renderer tex))))

(defun load-card-textures (renderer &optional (cards *cards*))
  (loop for card in cards
       collect (cons (card-name card) (load-card-texture renderer card))))

(defmethod initialize-instance :after ((w simple-window) &key &allow-other-keys)
  (setf (idle-render w) t)
  (sdl2-image:init '(:png))
  (sdl2-ttf:init)
  (setf *font* (sdl2-ttf:open-font *font-name* *font-size*))
  (setf *small-font* (sdl2-ttf:open-font *font-name* *small-font-size*))
  (with-slots (renderer sdl-window) w
    (setf renderer (sdl2:create-renderer sdl-window -1 '(:software)))

    (setf *default-pixel-format* (sdl2:get-window-pixel-format sdl-window))

    (setf *card-textures* (load-card-textures renderer *cards*))

    (setf (player-tex *player-1*) (load-texture-from-file renderer (asset-path "player-1.png")))
    (setf (player-tex *player-2*) (load-texture-from-file renderer (asset-path "player-2.png")))

    ;; (multiple-value-bind (format w h) (sdl2:get-current-display-mode 0)
    ;;   (declare (ignore format))
    ;;   (sdl2:set-window-size sdl-window (ash w -1) h))
    (sdl2:set-window-size sdl-window 1920 540)))

(defparameter *state* nil)

(defun set-state (state)
  (setf *state* state)
  (ecase *state*
    (:player-1 (start-turn *player-1*))
    (:player-2
     (make-avatar-turn *player-1*)
     (start-turn *player-2*)
     (make-ai-turn *player-2*)
     (make-avatar-turn *player-2*)
     (set-state :player-1))))

(defparameter *card-width* 96)
(defparameter *card-height* 128)

(defparameter *w* 0)
(defparameter *h* 0)

(defun get-card-tex (card)
  (cdr (assoc (card-name card) *card-textures*)))

(defun render-card (renderer card x y)
  (sdl2:set-render-draw-color renderer 255 0 0 255)
  (sdl2:render-draw-rect renderer
                         (sdl2:make-rect x y *card-width* *card-height*))
  (render-tex (get-card-tex card) x y))

(defun render-hand (renderer hand x y)
  (loop
     for card in hand
     do (render-card renderer card x y)
       (incf x *card-width*)))

(defun render-log (renderer x y)
  (loop
     for text in *game-log*
     do (let ((tex (load-texture-from-text renderer text)))
          (render-tex tex x y))
       (incf y *line-height*)))

(defparameter *hp-width* 120)

(defun render-hp (renderer hp x y)
  (sdl2:set-render-draw-color renderer 22 188 22 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect x y *hp-width* *line-height*))
  (let ((tex (load-texture-from-text renderer (format nil "Hp: ~a" hp))))
    (render-tex tex x y)))

(defun render-stats (renderer player x y)
  (let ((tex (load-texture-from-text renderer
                                     (with-slots (dmg resist evasion) (player-avatar player)
                                       (format nil "λ: ~a | dmg: ~a | ρ: ~a | ξ: ~a"
                                               (player-mana player) dmg resist evasion)))))
    (render-tex tex x y)))

(defmethod render ((window simple-window))
  ;; (handler-case
  (with-slots (rotation renderer) window
    (sdl2:set-render-draw-color renderer 33 33 33 255)
    (sdl2:render-clear renderer)
    (multiple-value-bind (w h) (window-size window)

      (let ((hp (avatar-hp (player-avatar *player-1*))))
        (render-hp renderer hp 0 (+ 4 *card-height*))
        (render-stats renderer *player-1*  (+ 4 *hp-width*) (+ 4 *card-height*)))

      (let ((hp (avatar-hp (player-avatar *player-2*))))
        (render-hp renderer hp (- w *hp-width*) (+ 4 *card-height*))
        (render-stats renderer *player-2* (- w 400 *hp-width*) (+ 4 *card-height*)))

      (let ((hand (player-hand *player-1*)))
        (render-hand renderer
                     hand
                     0
                     0))
      (let ((hand (player-hand *player-2*)))
        (render-hand renderer
                     hand
                     (- w (* (length hand) *card-width*))
                     (- h *card-height*)))

      (let ((tex (player-tex *player-1*)))
        (render-tex tex
                    0
                    (- (ash h -1) (ash (tex-height tex) -1))))

      (let ((tex (player-tex *player-2*)))
        (render-tex tex
                    (- w (tex-width tex))
                    (- (ash h -1) (ash (tex-height tex) -1))))

      (render-log renderer (round (* 0.35 w)) 0))
    (sdl2:render-present renderer)))
    ;; (error (err) (log-trace "Got render error ~a" err))))


(defun test-reload-text (text)
  (lambda (window)
    (with-slots (renderer texture) window
      (sdl2:set-render-draw-color renderer 33 33 33 255)
      (sdl2:render-clear renderer)
      (setf texture (load-texture-from-text renderer text)))))

(defmethod close-window ((window simple-window))
  (format t "Bye!~%")

  (loop for tex in *card-textures*
       do (sdl2:destroy-texture (tex-texture (cdr tex))))

  (with-slots (renderer) window
    (when renderer
      (sdl2:destroy-renderer renderer)))
  (sdl2-ttf:close-font *font*)
  (sdl2-ttf:close-font *small-font*)
  (sdl2-ttf:quit)
  (sdl2-image:quit)
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

;; (defmethod mousewheel-event ((window simple-window) ts x y)
;;   (with-slots (rotation) window
;;     (incf rotation (* 12 y))
;;     (render window)))

(defmethod textinput-event ((window simple-window) ts text)
  (format t "You typed: ~S~%" text))

(defmethod keyboard-event ((window simple-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (unless repeat-p
      (format t "~A ~S ~S~%" state scancode (sdl2:scancode-name scancode)))))

(defmethod mousebutton-event ((window simple-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y)
  (when (and (eq state :mousebuttonup) (eq *state* :player-1))
    (let ((hand (player-hand *player-1*))
          (index (floor (/ x *card-width*))))
      (if (< index (length hand))
          (let ((card (nth index hand)))
            (if (can-play-card *player-1* card)
                (play-card *player-1* card)
                (push-and-trace "Cannot play card ~a" (card-name card))))
          (set-state :player-2)))))

(defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  (when (> mask 0)
    (format t "Mouse motion, button-mask = ~A at ~A, ~A~%" mask x y)))

(defmethod controller-added-event ((window simple-window) c)
  (format t "Added ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

(defmethod controller-removed-event ((window simple-window) c)
  (format t "Removed ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

(defmethod controller-axis-motion-event ((window simple-window) c ts axis value)
  (format t "ID ~A, Axis ~A, Value ~A~%"
          (sdl2:game-controller-instance-id c) axis value))

(defmethod controller-button-event ((window simple-window) c state ts button)
  (format t "ID ~A, Button ~A, State ~S~%"
          (sdl2:game-controller-instance-id c) button state))

;; (make-instance 'simple-window)



(defclass tex ()
  ((renderer
    :initarg :renderer
    :initform (error "Must supply a renderer"))
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

(defun load-texture-from-text (renderer text &key (font *font*) (r 255) (g 255) (b 255) (a 255))
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-ttf:render-utf8-solid font text r g b a)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (setf texture (sdl2:create-texture-from-surface renderer surface))
        (sdl2:free-surface surface)))
    tex))

(defun set-color (tex r g b)
  (sdl2:set-texture-color-mod (tex-texture tex) r g b))

(defun render-tex (tex x y &key clip angle center flip)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy-ex renderer
                         texture
                         :source-rect clip
                         :dest-rect (sdl2:make-rect x
                                                    y
                                                    (if clip (sdl2:rect-width clip) width)
                                                    (if clip (sdl2:rect-height clip) height))
                         :angle angle
                         :center center
                         :flip (list (or flip :none)))))

(defun main()
  (kit.sdl2:start)
  (start-round)
  (set-state :player-1)
  (setf *window* (make-instance 'simple-window)))
