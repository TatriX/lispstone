;;;;

(asdf:defsystem #:twg
  :serial t
  :description "The game about parentheses"
  :author "TatriX <tatrics@gmail.com>"
  :license "MIT"

  :depends-on (:alexandria :sdl2kit :defpackage-plus :sdl2-image :sdl2-ttf)
  :pathname ""
  :serial t

  :components
  ((:file "main")
   (:file "test-window")))
