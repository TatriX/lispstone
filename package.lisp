(in-package :defpackage+-user-1)

(defpackage+ :lispstone
  (:use #:cl #:alexandria #:kit.sdl2 #:cl-locale)
  (:shadow :start)
  (:export #:main #:main-win))
