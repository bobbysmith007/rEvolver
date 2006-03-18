;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :rEvolver.system)
    (defpackage :rEvolver.system
      (:use :common-lisp :asdf))))

(in-package :rEvolver.system)

(defsystem :rEvolver
  :description "Playing around with genetic algorithms."
  :author "Nathan Bird <birdman@acceleration.net>"
  :licence "LGPL (or talk to me)"
  :version "0.1"
  :components
  ((:module :src
    :components ((:file "packages")
		 (:file "rEvolver" :depends-on ("packages"))
		 (:file "map" :depends-on ("rEvolver"))
		 (:file "interpreter" :depends-on ("rEvolver"))
		 (:file "dna-generator" :depends-on ("rEvolver"))
		 (:file "dna" :depends-on ( "dna-generator"))

		 (:file "World" :depends-on ( "map"))
		 (:file "Creature" :depends-on ("World" "dna" "interpreter"))
		 (:file "creature-environment" :depends-on ("Creature"))
		 (:file "simulation" :depends-on ("World" "creature-environment")))))
  :depends-on (:arnesi :adwcodebase))
