(in-package "COMMON-LISP-USER")

(defpackage :CSE
    (:use :common-lisp)
  (:export #:make-interpreter
	   #:interrupt-interpreter/cc
	   #:env-push))

(defpackage :rEvolver.map
    (:use :common-lisp)
  (:export #:rEvolver-map
	   #:2d-array-map
	   #:node
	   #:creatures-of
	   #:take-all-energy
	   #:find-node-xy
	   #:random-node
	   #:drop-random-energy
	   #:adjacent-nodes-of
	   #:adjacent-p))

(defpackage :revolver.dna.generator
    (:nicknames :generator)
  (:use :common-lisp
	:net.acceleration.utils
	)
  (:export
   #:generate-tree
   #:?start
   #:process-grammar-definition
   #:maybe-mutate-tree
   #:maybe-mutate-value))

(defpackage :revolver.dna
    (:nicknames :dna)
  (:export
    #:gamma
     #:lambda
     #:move
     #:feed
     #:energy?
     #:cons
     #:car
     #:cdr
     #:or
     #:if
     #:not
     #:eq
     #:equal
     #:node
     #:function
     #:list
     #:atom
     #:number
     #:nil
     #:t))

(defpackage :rEvolver
  (:use :common-lisp
	:net.acceleration.utils
	:net.acceleration.data-structures.trees
	:arnesi
	:rEvolver.map
	:CSE)
  (:import-from :generator #:generate-tree #:maybe-mutate-value #:maybe-mutate-tree)
  (:export #:creature-DNA))


