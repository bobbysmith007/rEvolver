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
	       (:file "map" :depends-on ("packages"))
	       (:file "interpreter" :depends-on ("packages"))
	       (:file "dna-generator" :depends-on ("packages"))
	       (:file "dna" :depends-on ("packages" "dna-generator"))
	       (:file "rEvolver" :depends-on ("packages"))
	       (:file "Creature" :depends-on ("packages" "rEvolver" "map" "dna" "dna-generator"))
	       (:file "World" :depends-on ("packages" "rEvolver" "map")))))
  :depends-on (:arnesi :adwcodebase))
