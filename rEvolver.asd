;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :rEvolver.system)
    (defpackage :rEvolver.system
      (:use :common-lisp :asdf))))

(in-package :rEvolver.system)

(defsystem :rEvolver
  :description "Playing around with genetic algorithms."
  :author "Nathan Bird <birdman@acceleration.net>"
  :licence "GPL (or talk to me)"
  :version "0.1"
  :components
  ((:module :src
    :components ((:file "packages")
                 (:file "Creature" :depends-on ("packages" "rEvolver" "map"))
		 (:file "map" :depends-on ("packages"))
                 (:file "rEvolver" :depends-on ("packages"))
                 (:file "World" :depends-on ("packages" "rEvolver")))))
  :depends-on ())
