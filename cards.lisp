(in-package :lispstone)

(defclass card ()
  ((name :initarg :name
         :accessor card-name)
   (cost :initarg :cost
         :accessor card-cost)
   (desc :initarg :desc
         :accessor card-desc)
   (effect :initarg :effect
           :accessor card-effect
           :initform (error "Must supply an effect"))))

(defmethod print-object ((card card) stream)
  (with-slots (name cost desc) card
    (format stream "~a | cost ~a | ~a" name cost desc)))

(defparameter *cards* (list (make-instance 'card
                                           :name "()"
                                           :cost 1
                                           :desc "+5 hp"
                                           :effect (lambda (avatar)
                                                     (heal avatar 5)))
                            (make-instance 'card
                                           :name "[]"
                                           :cost 2
                                           :desc "+2 dmg"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-dmg avatar) 2)))
                            (make-instance 'card
                                           :name "{}"
                                           :cost 3
                                           :desc "+10% ρ"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-resist avatar) 10)))
                            (make-instance 'card
                                           :name "<>"
                                           :cost 4
                                           :desc "+10% ξ"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-evasion avatar) 5)))
                            (make-instance 'card
                                           :name "⊂⊃"
                                           :cost 5
                                           :desc "+50 hp"
                                           :effect (lambda (avatar)
                                                     (heal avatar 50)))
                            (make-instance 'card
                                           :name "≺≻"
                                           :cost 6
                                           :desc "+20 dmg"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-dmg avatar) 20)))
                            (make-instance 'card
                                           :name "⊏⊐"
                                           :cost 7
                                           :desc "+50% ρ,ξ"
                                           :effect (lambda (avatar)
                                                     (incf (avatar-resist avatar) 50)
                                                     (incf (avatar-evasion avatar) 50)))))

(defun find-card-by-name (name &optional (cards *cards*))
  (find name cards :key #'card-name :test 'equal))

(defmethod print-object ((card card) stream)
  (with-slots (name cost desc) card
    (format stream "~a | cost ~a | ~a" name cost desc)))

(defun find-card-by-name-or-index (player name-or-index)
  (handler-case
      (let ((index (parse-integer name-or-index)))
        (nth index (player-hand player)))
    (error () (find-card-by-name name-or-index))))

(defun get-card-tex (card)
  (cadr (assoc (card-name card) *card-textures*)))

(defun get-card-hover-tex (card)
  (caddr (assoc (card-name card) *card-textures*)))

(defun render-card (card x y)
  (render-tex (get-card-tex card) x y))

(defun render-card-hover (card x y)
  (render-tex (get-card-hover-tex card) x y))

;; (defun render-card (renderer card x y)
;;   (sdl2:set-render-draw-color renderer 255 0 0 255)
;;   (sdl2:render-draw-rect renderer
;;                          (sdl2:make-rect x y *card-width* *card-height*))
;;   (render-tex (get-card-tex card) x y))

(defparameter *card-textures* nil)
(defparameter *card-width* 96)
(defparameter *card-height* 128)

(defun load-card-textures (renderer &optional (cards *cards*))
  (loop for card in cards
     for i = 1 then (1+ i)
     collect (list (card-name card)
                   (load-texture-from-file renderer (asset-path (format nil "~a.png" i)))
                   (load-texture-from-file renderer (asset-path (format nil "~a-hover.png" i))))))


(defun %load-card-texture (renderer card)
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
