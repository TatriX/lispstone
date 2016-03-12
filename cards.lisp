(in-package :lispstone)

(defparameter *card-textures* nil)

(defun random-card (&optional (cards *cards*))
  (let ((prototype (random-elt cards)))
    (with-slots (id name cost buff-desc buff debuff-desc debuff) prototype
      (make-instance 'card
                     :id id
                     :name name
                     :cost cost
                     :buff-desc buff-desc
                     :buff buff
                     :debuff-desc debuff-desc
                     :debuff debuff))))

(defun available-cards ()
  (remove-if #'card-bought *cards*))

(eval-when (:compile-toplevel)
  (defparameter *effect-abbrs* '((hp . "hp")
                                 (dmg . "ψ")
                                 (resist . "ρ")
                                 (evasion . "ξ")))

  (defun effect-abbr (symbol)
    (cdr (assoc symbol *effect-abbrs*)))

  (defun parse-buff-desc (buff)
    (format nil "~{~A~^ ~}"
            (loop for (field value) on buff by #'cddr
               collect (format nil "~a~a" value (effect-abbr field)))))

  (defun parse-buff-effect (buff op)
    (loop for (field value) on buff by #'cddr
       collect `(setf (,(symbolicate "AVATAR-" field) avatar)
                      (max 0 (,op (,(symbolicate "AVATAR-" field) avatar) ,value)))))

  (defun make-buff-effect (buff op)
    (when buff
      `(lambda (avatar)
         ,@(parse-buff-effect buff op)))))

(defmacro init-cards (&rest cards)
  `(list
    ,@(loop
         for fields in cards
         for i = 1 then (1+ i)
         collect `(make-instance 'card
                                 ,@(destructuring-bind (name &key cost buff debuff free) fields
                                                       `(:id ,i
                                                             :name ,name
                                                             :cost ,cost
                                                             :buff-desc ,(parse-buff-desc buff)
                                                             :buff ,(make-buff-effect buff '+)
                                                             :debuff-desc ,(parse-buff-desc debuff)
                                                             :debuff ,(make-buff-effect debuff '-)
                                                             :bought ,free))))))

(defparameter *card-file-names* '(("<>" . "1")
                                  ("()" . "2")
                                  ("[]" . "3")
                                  ("{}" . "4")
                                  ("⊂⊃" . "5")
                                  ("⊏⊐" . "6")
                                  ("≺≻" . "7")))

(defparameter *cards*
  (init-cards
   ("(⊃"
    :buff (hp 15)
    :debuff (hp 15)
    :cost 3)
   ("()"
    :buff (hp 30)
    :cost 4)
   ("(]"
    :buff (dmg 2 hp 10)
    :cost 3)
   ("(}"
    :buff (hp 5 resist 3)
    :cost 1)
   ("(>"
    :buff (hp 5 evasion 5)
    :cost 3)
   ("(⊐"
    :buff (hp 1)
    :cost 5)
   ("(≻"
    :buff (hp 100)
    :cost 10)
   ("[]"
    :buff (dmg 10)
    :cost 4)
   ("[}"
    :debuff (hp 2)
    :cost 7)
   ("[>"
    :buff (hp 1)
    :cost 5)
   ("{}"
    :buff (resist 15)
    :cost 1)
   ("[⊃"
    :buff (dmg 5)
    :debuff (hp 15)
    :cost 2)
   ("[⊐"
    :debuff (hp 1)
    :cost 5)
   ("[≻"
    :debuff (hp 1)
    :cost 5)
   ("{>"
    :buff (resist 15 evasion 15)
    :cost 4)
   ("{⊃"
    :buff (resist 5)
    :debuff (hp 5)
    :cost 1)
   ("{⊐"
    :buff (resist 5)
    :debuff (dmg 2)
    :cost 1)
   ("{≻"
    :debuff (evasion 10 resist 10)
    :cost 3)
   ("<>"
    :buff (evasion 15)
    :cost 2)
   ("<⊃"
    :buff (evasion 5)
    :debuff (hp 5)
    :cost 1)
   ("<≻"
    :buff (hp 2)
    :cost 7)
   ("⊂⊃"
    :debuff (hp 30)
    :cost 6)
   ("<⊐"
    :buff (hp 2)
    :cost 7)
   ("⊂⊐"
    :debuff (hp 1)
    :cost 5)
   ("⊂≻"
    :buff (hp 1)
    :cost 5)
   ("⊏⊐"
    :debuff (dmg 10)
    :cost 3)
   ("⊏≻"
    :buff (hp 5 dmg 5)
    :debuff (evasion 10 resist 10)
    :cost 5)
   ("≺≻"
    :debuff (evasion 20 resist 20)
    :cost 5)))

(loop
   for card in *cards*
   for i = 0 then (1+ i)
   for x = 10 then (+ x 10)
   when (zerop (rem i 7)) do
     (setf x 10)
   end
   do (with-slots (price) card
        (setf price x)))
