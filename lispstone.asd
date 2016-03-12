;;;;

(asdf:defsystem #:lispstone
  :serial t
  :description "Lispstone — the card game about parentheses"
  :author "TatriX <tatrics@gmail.com>"
  :license "MIT"

  :depends-on (:alexandria
               :sdl2kit
               :defpackage-plus
               :sdl2-image
               :sdl2-ttf
               :sdl2-mixer
               :cl-locale)
  :pathname ""
  :serial t

  :components
  ((:file "package")
   (:file "utils")

   (:file "tex")
   (:file "fonts")

   (:file "config")

   (:file "entity")
   (:file "text-entity")


   (:file "card")
   (:file "entities/card")

   (:file "entities/bonus")

   (:file "log")
   (:file "entities/log")

   (:file "player")
   (:file "entities/player")

   (:file "enemy")
   (:file "entities/enemy")

   (:file "deck")
   ;; (:file "entities/deck")

   (:file "hand")
   (:file "entities/hand")

   (:file "avatar")
   (:file "entities/avatar")

   (:file "cards")
   (:file "enemies")

   (:file "help")

   (:file "scene")
   (:file "scenes/table")
   (:file "scenes/ladder")
   (:file "scenes/shop")
   (:file "scenes/luck")
   (:file "scenes/game-over")

   (:file "test-window")
   (:file "game")

   (:file "main")))
