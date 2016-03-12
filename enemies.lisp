(in-package :lispstone)

(defun init-enemies (enemies)
  (loop for data in enemies
     collect (make-instance 'enemy
                            :name (car data)
                            :avatar-proto (apply #'make-instance 'avatar :name (car data) (cdr data)))))

(defparameter *enemies* (init-enemies '(("python"
                                         :hp 100
                                         :dmg 10
                                         :resist 0
                                         :evasion 0)
                                        ("go"
                                         :hp 150
                                         :dmg 12
                                         :resist 5
                                         :evasion 5)
                                        ("github"
                                         :hp 200
                                         :dmg 14
                                         :resist 10
                                         :evasion 10)
                                        ("freebsd"
                                         :hp 250
                                         :dmg 16
                                         :resist 15
                                         :evasion 15)
                                        ("android"
                                         :hp 300
                                         :dmg 18
                                         :resist 40
                                         :evasion 0)
                                        ("php"
                                         :hp 300
                                         :dmg 20
                                         :resist 0
                                         :evasion 40)
                                        ("linux"
                                         :hp 350
                                         :dmg 25
                                         :resist 30
                                         :evasion 30))))
