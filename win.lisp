(load "lispstone.asd")
(ql:quickload :lispstone)

(defun build ()
  (sb-ext:save-lisp-and-die "lispstone.exe"
                            :toplevel #'lispstone:main-win
                            :executable t
                            :application-type :gui))
