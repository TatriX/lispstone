(setf sb-impl::*default-external-format* :UTF-8)

(in-package :lispstone)

(defvar *window* nil)
(defvar *debug* nil)

(defparameter *default-pixel-format* nil)

(defclass simple-window (test-window)
  ((renderer :accessor window-renderer)
   (scene :accessor window-scene)))

(defun create-renderer (sdl-window)
  (handler-case
      (sdl2:create-renderer sdl-window -1 '(:accelerated))
    (error () (sdl2:create-renderer sdl-window -1 '(:software)))))

(defparameter *music* nil)
(defparameter *music-playing* nil)

(defparameter *lose-tex* nil)
(defparameter *win-tex* nil)
(defparameter *round-end-tex* nil)

(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)

(defmethod initialize-instance :after ((w simple-window) &key &allow-other-keys)
  (setf (idle-render w) t)
  (sdl2-image:init '(:png))
  (sdl2-ttf:init)

  (sdl2-mixer:init :ogg)
  (sdl2-mixer:open-audio 48000 :s16sys 1 1024)

  (setf *music* (sdl2-mixer:load-music (asset-path "song.ogg")))
  (sdl2-mixer:volume -1 50)
  (sdl2-mixer:play-music *music*)
  (setf *music-playing* t)

  (let ((font-path (asset-path *font-name*)))
    (setf *font* (sdl2-ttf:open-font font-path *font-size*)
          *small-font* (sdl2-ttf:open-font font-path *small-font-size*)))
  (with-slots (scene renderer sdl-window) w
    (setf renderer (create-renderer sdl-window))

    (setf *default-pixel-format* (sdl2:get-window-pixel-format sdl-window))

    (setf *card-textures* (load-card-textures renderer *cards*))

    (setf *lose-tex* (load-texture-from-text renderer "You lost, faggot." :r 200 :g 50 :b 50))
    (setf *win-tex* (load-texture-from-text renderer "You won, faggot." :r 50 :g 200 :b 50))

    ;; (sdl2:set-window-fullscreen sdl-window :desktop)
    ;; (multiple-value-bind (w h) (window-size w)
    ;;   (setf scene (make-instance 'scene
    ;;                              :width w
    ;;                              :height h
    ;;                              :renderer renderer)))


    (sdl2:set-window-size sdl-window *screen-width* *screen-height*)
    (setf scene (make-instance 'scene
                               :width *screen-width*
                               :height *screen-height*
                               :renderer renderer))
    (load-scene scene)))

(defparameter *state* nil)

(defun win (player)
  (if (equalp *player-1* player)
      (progn
        (incf (car *score*))
        (setf *round-end-tex* *win-tex*))
      (progn
        (incf (cadr *score*))
        (setf *round-end-tex* *lose-tex*)))
  (set-state :round-end))

(defun set-state (state)
  (setf *state* state)
  (ecase *state*
    (:player-1 (start-turn *player-1*))
    (:player-2
     (make-avatar-turn *player-1*)
     (start-turn *player-2*)
     (make-ai-turn *player-2*)
     (make-avatar-turn *player-2*)
     (cond
       ((dead-p *player-1*)
        (win *player-2*))
       ((dead-p *player-2*)
        (win *player-1*))
       (t (set-state :player-1))))
    (:round-end
     (format t "END"))))

(defmethod render ((window simple-window))
  ;; (handler-case
  (with-slots (scene renderer) window
    (sdl2:set-render-draw-color renderer 33 33 33 255)
    (sdl2:render-clear renderer)
    (case *state*
      (:round-end
       (render-tex *round-end-tex*
                   (- (ash *screen-width* -1) (ash (tex-width *round-end-tex*) -1))
                   (- (ash *screen-height* -1) (ash (tex-height *round-end-tex*) -1))))
      (t (render-scene scene)))
    (sdl2:render-present renderer)))

(defmethod close-window ((window simple-window))
  (format t "Bye!~%")

  (with-slots (renderer) window
    (when renderer
      (sdl2:destroy-renderer renderer)))

  (sdl2-mixer:halt-music)
  (sdl2-mixer:close-audio)
  (sdl2-mixer:free-music *music*)
  (sdl2-mixer:quit)

  (sdl2-ttf:close-font *font*)
  (sdl2-ttf:close-font *small-font*)
  (sdl2-ttf:quit)

  (sdl2-image:quit)

  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

;; (defmethod window-event ((window simple-window) (type (eql :resized)) timestamp data1 data2)
;;     (multiple-value-bind (format w h) (sdl2:get-current-display-mode 0)
;;       (declare (ignore format))
;;       (sdl2:set-window-size (sdl-window window) w h)))

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
  (when (eq state :mousebuttonup)
    (case *state*
      (:player-1
       (let ((entity (find-entity-by (window-scene window) :coords (list x y))))
         (typecase entity
           (hand-entity
            (with-slots (hand) entity
              (%with-entity (tex ex ey ew eh) entity
                (declare (ignore ey))
                (let ((index (floor (/ (- x ex) *card-width*))))
                  (if (< index (hand-size hand))
                      (let ((card (nth index (hand-cards hand))))
                        (if (can-play-card *player-1* card)
                            (play-card *player-1* card)
                            (push-and-trace "Cannot play card ~a" (card-name card)))))))))
           (entity (case (entity-id entity)
                     (:mute (if *music-playing*
                                (progn (setf *music-playing* nil)
                                       (sdl2-mixer:halt-music))
                                (progn (setf *music-playing* t)
                                       (sdl2-mixer:play-music *music*))))
                     (:end-turn (set-state :player-2)))))))
      (:round-end (start-round)
                  (set-state :player-1)))))



(defparameter *hovered-entity* nil)
(defparameter *hovered-card* -1)

(defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  (case *state*
    (:player-1
     (let ((entity (find-entity-by (window-scene window) :coords (list x y))))
       (typecase entity
         (hand-entity
          (with-slots (hand) entity
            (%with-entity (tex ex ey ew eh) entity
              (declare (ignore ey))
              (let ((index (floor (/ (- x ex) *card-width*))))
                (if (< index (hand-size hand))
                    (setf *hovered-card* index)
                    (setf *hovered-card* -1))))))
         (t (setf *hovered-card* -1))))
     ;; (let ((entity (find-entity-by (window-scene window) :coords (list x y))))
     ;;   (unless (equal *hovered-entity* entity)
     ;;     (entity-onmouseout *hovered-entity*)
     ;;     (setf *hovered-entity* entity)
     ;;     (when entity
     ;;       (entity-onmouseover entity))))
     ))
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

(defun start ()
  (kit.sdl2:start)
  (start-round)
  (set-state :player-1)
  (setf *window* (make-instance 'simple-window)))

(defun main ()
  (setf *debug* t)
  (start))

(defun main-win ()
  (setf *debug* nil)
  (start)
  (sdl2:make-this-thread-main))
