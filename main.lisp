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

(in-package :twg)

(defclass simple-window (test-window)
  ((renderer :initform nil
             :accessor window-renderer)
   (texture :initform nil)
   (update-queue :initform nil
                 :accessor update-queue)))

;;; All of these methods are OPTIONAL.  However, without a render
;;; method, your window will not look like much!


;;; Note this is an :AFTER method.  You should either use :AFTER, or
;;; you must (CALL-NEXT-METHOD).

(defparameter *font* nil)

(defmethod initialize-instance :after ((w simple-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.
  (sdl2-image:init '(:png))
  (sdl2-ttf:init)
  (setf *font* (sdl2-ttf:open-font "Pacifico.ttf" 28))
  (setf (idle-render w) t)
  (with-slots (renderer sdl-window texture) w
    (setf renderer (sdl2:create-renderer sdl-window -1 '(:accelerated)))
    (setf texture (load-texture-from-text renderer "Fuck you!"))))

(defmethod render ((window simple-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW
  ;; after RENDER.
  (with-slots (rotation renderer texture update-queue) window
    (loop for callback in update-queue
       do (funcall callback window)
         finally (setf update-queue nil))
    (sdl2:set-render-draw-color renderer 33 33 33 255)
    (sdl2:render-clear renderer)
    (render-tex texture 0 0)
    (sdl2:render-present renderer)))

(defun test-reload-text (text)
  (lambda (window)
    (with-slots (renderer texture) window
      (sdl2:set-render-draw-color renderer 33 33 33 255)
      (sdl2:render-clear renderer)
      (setf texture (load-texture-from-text renderer text)))))

(defmethod close-window ((window simple-window))
  (format t "Bye!~%")
  (sdl2:destroy-renderer (window-renderer window))
  (sdl2-ttf:quit)
  (sdl2-image:quit)
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod mousewheel-event ((window simple-window) ts x y)
  (with-slots (rotation) window
    (incf rotation (* 12 y))
    (render window)))

(defmethod textinput-event ((window simple-window) ts text)
  (format t "You typed: ~S~%" text))

(defmethod keyboard-event ((window simple-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (unless repeat-p
      (format t "~A ~S ~S~%" state scancode (sdl2:scancode-name scancode)))))

(defmethod mousebutton-event ((window simple-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

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

(defun load-texture-from-text (renderer text)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-ttf:render-text-solid *font* text 255 255 255 255)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
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
                         :flip (list flip))))


(defclass card ()
  ((name :initarg :name
         :accessor card-name)
   (cost :initarg :cost
         :accessor card-cost)
   (desc :initarg :desc
         :accessor card-desc)))

(defparameter *cards* (list (make-instance 'card :name "()" :cost 1 :desc "+10hp")
                            (make-instance 'card :name "[]" :cost 2 :desc "+2dmg")
                            (make-instance 'card :name "{}" :cost 3 :desc "+10% resist")
                            (make-instance 'card :name "<>" :cost 4 :desc "+10% evasion")
                            (make-instance 'card :name "⊂⊃" :cost 5 :desc "+50hp")
                            (make-instance 'card :name "≺≻" :cost 6 :desc "+10dmg")
                            (make-instance 'card :name "⊏⊐" :cost 7 :desc "+10% resist +10% evasion")))

(defparameter *base-hp* 100)
(defparameter *base-dmg* 10)

(defclass avatar ()
  ((hp :initform *base-hp*
       :accessor avatar-hp)
   (dmg :initform *base-dmg*
        :accessor avatar-dmg)
   (resist :initform 0
           :accessor avatar-resist)
   (evasion :initform 0
            :accessor avatar-evasion)))

(defmethod print-object ((ava avatar) stream)
  (with-slots (hp dmg resist evasion) ava
    (format stream "~&| hp ~3a | dmg ~2a | resist ~a | evasion ~a |"
            hp dmg resist evasion)))

(defclass player ()
  ((avatar :accessor player-avatar)
   (mana :accessor player-mana)
   (hand :accessor player-hand)
   (deck :accessor player-deck)
   (name :accessor player-name
         :initarg :name
         :initform (error "Must supply a name"))))

(defun random-deck ()
  (loop for i below *deck-size*
     collect (random-elt *cards*)))

(defun reset-player (player)
  (with-slots (hand deck mana avatar) player
    (setf mana 0
          hand '()
          deck (random-deck)
          avatar (make-instance 'avatar))))

(defmethod initialize-instance :after ((pl player) &key)
  (reset-player pl))

(defmethod print-object ((pl player) stream)
  (with-slots (name mana deck hand) pl
    (format stream "~&| ~a | mana ~2d | hand ~(~a~) | deck ~d |"
            name mana (length hand) (length deck))))

(defparameter *deck-size* 30)
(defparameter *max-deck-size* 5)

(defparameter *game* nil)
(defparameter *player-1* (make-instance 'player :name "Илья"))
(defparameter *player-2* (make-instance 'player :name "Мальчик"))
(defparameter *players* (list *player-1* *player-2*))
(defparameter *max-mana* 10)

(defun start-round ()
  (mapc #'reset-player *players*))

(defun start-turn (player)
  (with-slots (mana hand deck) player
    (when (< mana *max-mana*)
      (incf mana))
    (when deck
      (let ((card (pop deck)))
        (when (< (length hand) *max-deck-size*))
        (push card hand)))))

(defparameter *trace-enabled* t)

(defun log-trace (fmt &rest args)
  (when *trace-enabled*
    (apply #'format *standard-output* fmt args)))

(defun make-turn (player)
  (log-trace "~&Turn of ~a~%" (player-name player)))

(defun other-player (player &optional (players *players*))
  (if (equal player (car players))
      (cadr players)
      (car players)))

(defun can-play-card (player card)
  (>= (player-mana player) (card-cost card)))

(defun play-card (player card)
  (log-trace "~&~a playing ~a card" (player-name player) (card-name card))
  (with-slots (hp dmg resist evasion) (player-avatar player)
    (case (card-name card)
      ("()" (incf hp 2))
      ("[]" (incf dmg 5))
      ("{}" (incf resist 10))
      ("<>" (incf evasion 10))
      ("⊂⊃" (incf hp 10))
      ("≺≻" (incf dmg 20))
      ("⊏⊐" (incf resist 10) (incf evasion 10)))))

(defun roll-dice ()
  (random 100))

(defun lucky (value)
  (< (roll-dice) value))

(defun hit (attacker defending)
  (with-slots (hp resist evasion) defending
    (if (lucky evasion)
        'miss
        (let ((dmg (* (avatar-dmg attacker) 0.01 (- 100 resist))))
          (decf hp dmg)
          dmg))))

(defun update-avatar (avatar)
  (with-slots (dmg resist evasion) avatar
    (when (> dmg *base-dmg*)
      (decf dmg))
    (setf resist (ash resist -1))
    (setf evasion (ash evasion -1))))

(defun make-avatar-turn (player)
  (with-accessors ((player-name player-name) (player-avatar player-avatar)) player
    (with-accessors ((enemy-name player-name) (enemy-avatar player-avatar)) (other-player player)
      (let ((result (hit player-avatar enemy-avatar)))
        (log-trace "~&~a: ~a" player-name player-avatar)
        (if (eq result 'miss)
            (log-trace "~&~a's avatar missed" player-name)
            (log-trace "~&~a's avatar made ~a damage" player-name result))
        (log-trace "~&~a: ~a" enemy-name enemy-avatar)
        (update-avatar player-avatar)
        (update-avatar enemy-avatar)))))

(defun find-playable-card (player)
  (let ((hand (player-hand player)))
    (when hand
      (find-if (lambda (card) (can-play-card player card))
               (shuffle hand)))))

(defun make-ai-turn (player)
  (log-trace "~&~%AI Turn of ~a~%" (player-name player))
  (let ((card (find-playable-card player)))
    (if card
        (play-card player card)
        (log-trace "~&~a has no cards to play" (player-name player)))))

(defun next-turn ()
  (start-turn *player-1*)
  (make-ai-turn *player-1*)
  (make-avatar-turn *player-1*)

  (start-turn *player-2*)
  (make-ai-turn *player-2*)
  (make-avatar-turn *player-2*)

  (log-trace "~&***~% ~a" *players*))

(defun test()
  (dotimes (i 30) (next-turn)))
