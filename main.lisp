;; (setf sb-impl::*default-external-format* :UTF-8)
(in-package :lispstone)

(proclaim '(optimize (speed 3)))

(defvar *window* nil)
(defvar *debug* nil)

(defparameter *default-pixel-format* nil)

(defclass simple-window (test-window)
  ((renderer :accessor window-renderer)
   (scene
    :initform nil
    :accessor window-scene)
   (scenes
    :initform '()
    :accessor window-scenes)))

(defun create-renderer (sdl-window)
  (handler-case
      (sdl2:create-renderer sdl-window -1 '(:accelerated))
    (error () (sdl2:create-renderer sdl-window -1 '(:software)))))

(defparameter *music* nil)
(defparameter *music-playing* nil)


(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)

(defmethod initialize-instance :after ((w simple-window) &key &allow-other-keys)
  (setf (idle-render w) t)

  (load-fonts)

  (with-slots (renderer sdl-window) w
    (setf renderer (create-renderer sdl-window))

    (setf *default-pixel-format* (sdl2:get-window-pixel-format sdl-window))

    (load-card-entities renderer *cards*)

    (load-scenes w)

    (start-music)
    (set-state :ladder w)))

(defun load-scenes (window)
  (loop for scene in '(table-scene
                       luck-scene
                       ladder-scene
                       shop-scene
                       game-over-scene)
     do (add-scene window (make-instance scene
                                    :width *screen-width*
                                    :height *screen-height*
                                    :renderer (window-renderer window)))))

(defun load-fonts ()
  (let ((font-path (asset-path *font-name*)))
    (setf *font* (sdl2-ttf:open-font font-path *font-size*)
          *small-font* (sdl2-ttf:open-font font-path *small-font-size*))))

(defun add-scene (window scene)
  (push scene (window-scenes window)))

(defun find-scene (window name)
  (find name (window-scenes window) :key #'scene-name))

(defun set-scene (window name)
  (with-slots ((current scene)) window
    (let ((scene (find-scene window name)))
      (unless scene
        (error "Scene ~a not found" name))
      (scene-onswitch scene)
      (setf current scene))))


(defun start-music ()
  (setf *music* (sdl2-mixer:load-music (asset-path "plain.ogg")))
  (sdl2-mixer:volume -1 50)

  (unless *debug*
    (sdl2-mixer:play-music *music*)
    (setf *music-playing* t)))

(defparameter *state* nil)

(defun game-over ()
  (set-state :game-over))

(defun round-end-splash (id)
  (start-round)
  (set-state :ladder)
  (let ((scene (find-scene *window* :ladder-scene)))
    (with-slots (width height) scene
      (entity-fade-out (add-to-scene-and-load scene (make-instance 'entity
                                                                   :id id
                                                                   :x (ash width -1)
                                                                   :y (ash height -1)
                                                                   :anchor-x :center
                                                                   :anchor-y :center)) 1.5))))
(defun player-wins ()
  (incf (car *score*))
  (next-enemy)
  (if (>= *current-enemy* (length *enemies*))
      (game-over)
      (round-end-splash :win)))

(defun enemy-wins ()
  (incf (cadr *score*))
  (round-end-splash :lost))

(defun set-state (state &optional (window *window*))
  (log-trace "New state ~a~%" state)
  (setf *state* state)
  (ecase *state*
    (:ladder
     (set-scene window :ladder-scene))
    (:shop
     (set-scene window :shop-scene))
    (:delay)
    (:round-start
     (set-scene window :table-scene)
     (set-state :player))
    (:player (start-turn *player*))
    (:player-attack
     (set-scene window :luck-scene))
    (:player-attack-animation
     (set-scene window :table-scene))
    (:player-luck
     (with-slots (luck name) *player*
       (if luck
           (progn
             (push-and-trace "Fortune loves you, ~a!" name)
             (setf luck nil)
             (set-state :player-attack-animation))
           (progn
             (make-ai-turn *enemy*)
             (set-state :enemy)))))
    (:enemy)
    (:enemy-attack-animation)
    (:game-over
     (set-scene window :game-over-scene))))

(defmethod render ((window simple-window))
  ;; (handler-case
  (with-slots (scene renderer) window
    (sdl2:set-render-draw-color renderer 11 11 11 255)
    (sdl2:render-clear renderer)
    (update-scene scene)
    (render-scene scene)
    (sdl2:render-present renderer)))

(defmethod close-window ((window simple-window))
  (sdl2-mixer:halt-music)
  (sdl2-mixer:free-music *music*)

  (call-next-method)
  (unless *debug*
    (deinit)))

(defmethod mousebutton-event ((window simple-window) state ts b x y)
  (when (eq state :mousebuttondown)
    (case *state*
      (:player-attack-animation)
      (:enemy-attack-animation)
      (t
       (let ((entity (find-entity-by (window-scene window) :coords (list x y))))
         (when entity
           (entity-onclick entity)
           (case (entity-id entity)
             (:mute (if *music-playing*
                        (progn (setf *music-playing* nil)
                               (sdl2-mixer:halt-music))
                        (progn (setf *music-playing* t)
                               (sdl2-mixer:play-music *music*))))
             (:end-turn (set-state :player-attack)))))))))

(defparameter *hovered-entity* nil)

(defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  (let ((entity (find-entity-by (window-scene window) :coords (list x y))))
    (unless (equal *hovered-entity* entity)
      (when *hovered-entity*
        (entity-onmouseout *hovered-entity*))
      (setf *hovered-entity* entity)
      (when entity
        (entity-onmouseover entity)))))

(defvar *was-init* nil)

(defun init ()
  (unless *was-init*
    (setf *was-init* t)
    (sdl2-image:init '(:png))
    (sdl2-ttf:init)
    (sdl2-mixer:init :ogg)
    (sdl2-mixer:open-audio 44100 :s16sys 1 1024)))

(defun deinit ()
  (when *was-init*
    (setf *was-init* nil)

    (sdl2-mixer:quit)

    (sdl2-ttf:close-font *font*)
    (sdl2-ttf:close-font *small-font*)
    (sdl2-ttf:quit)

    (sdl2-image:quit)
    (setf *window* nil)
    (sdl2.kit:quit)))

(defun start ()
  (define-dictionary dict
    (:ru-RU (asset-path "i18n/ru_RU/dict.lisp")))
  (setf *random-state* (make-random-state t))
  (setf (current-dictionary) :dict)
  (setf *locale* :ru-RU)

  (init)
  (init-tex-cache)
  (start-round)
  (setf *window* (make-instance 'simple-window
                                :title "Lisptone"
                                :w *screen-width*
                                :h *screen-height*
                                :borderless t)))

(defun main ()
  (setf *debug* t)
  (sdl2.kit:start)
  (start))

(defun main-win ()
  (setf *debug* nil)
  (sdl2.kit:with-start (:this-thread-p t)
    (start)))
