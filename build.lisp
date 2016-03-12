(load "lispstone.asd")
(ql:quickload :lispstone)

;; (defun build ()
;;   (sb-ext:save-lisp-and-die "lispstone.exe"
;;                             :toplevel #'lispstone:main-win
;;                             :executable t
;;                             :application-type :gui))
;; (defun build-linux ()
;;   (sb-ext:save-lisp-and-die "bin/lispstone"
;;                             :toplevel #'lispstone:main-win
;;                             :executable t))
