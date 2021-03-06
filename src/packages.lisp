(in-package "COMMON-LISP-USER")

(defpackage :CSE
    (:use :common-lisp)
  (:export #:make-interpreter
	   #:interrupt-interpreter/cc
	   #:escape
	   #:reason
	   #:code-error
	   #:invalid-gamma-application
	   #:unbound-name
	   #:append-to-environment))

;(defpackage :rEvolver.map
;    (:use :common-lisp)
;  (:export #:rEvolver-map
;	   #:2d-array-map
;	   #:node
;	   #:creatures-of
;	   #:take-all-energy
;	   #:add-energy
;	   #:find-node-xy
;	   #:random-node
;	   #:drop-random-energy
;	   #:adjacent-nodes-of
;	   #:adjacent-p))

(defpackage :revolver.dna
    (:nicknames :dna)
  (:export
   #:EOF
   #:gamma
   #:lambda
   #:look-at
   #:move
   #:feed
   #:energy
   #:energy?
   #:cons
   #:car
   #:cdr
   #:or
   #:creatures
   #:if
   #:not
   #:eq
   #:equal
   #:node
   #:function
   #:list
   #:atom
   #:number
   #:asexually-reproduce
   #:nil
   #:t
   #:me
   #:<
   #:+
   #:-))

(defpackage :rEvolver
    (:use :common-lisp
	  :net.acceleration.utils
	  :net.acceleration.data-structures.trees
          :arnesi
	  :CSE)
  (:shadowing-import-from :net.acceleration.data-structures.trees #:queue #:dequeue 
    )
  (:shadow #:Y)
  (:export #:creature-DNA
	   #:*simulation*
	   #:*world*
	   #:*simulation*
	   #:*golem*
	   #:creature-count
	   #:population-infusions
	   #:repopulation-infusions
	   #:revolver-map
	   #:tick-number
	   #:free-energy
	   #:animation-count
	   ))



;(defpackage :revolver.dna.generator
;    (:nicknames :generator)
;  (:use :common-lisp
;	:net.acceleration.utils
;	:revolver
;	:arnesi
;	)
;  (:shadow #:x #:y)
;  (:export
;   #:generate-tree
;   #:?start
;   #:process-grammar-definition
;   #:maybe-mutate-tree
;   #:maybe-mutate-value))

