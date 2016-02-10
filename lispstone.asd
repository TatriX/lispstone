;;;;

(asdf:defsystem #:lispstone
  :serial t
  :description "Lispstone — the card game about parentheses"
  :author "TatriX <tatrics@gmail.com>"
  :license "MIT"

  :depends-on (:alexandria :sdl2kit :defpackage-plus :sdl2-image :sdl2-ttf :sdl2-mixer)
  :pathname ""
  :serial t

  :components
  ((:file "package")
   (:file "utils")

   (:file "tex")
   (:file "fonts")

   (:file "entity")

   (:file "cards")
   (:file "entities/card")

   (:file "log")
   (:file "entities/log")

   (:file "player")
   (:file "entities/player")

   (:file "deck")
   ;; (:file "entities/deck")

   (:file "hand")
   (:file "entities/hand")

   (:file "avatar")
   (:file "entities/avatar")

   (:file "scene")
   (:file "test-window")
   (:file "game")

   (:file "main")))
