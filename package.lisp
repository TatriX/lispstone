(in-package :defpackage+-user-1)

(defpackage+ :lispstone
  (:use #:cl #:alexandria #:kit.sdl2)
  (:shadow :start)
  (:export #:main #:main-win))
