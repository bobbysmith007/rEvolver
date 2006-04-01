(in-package :rEvolver)
(defvar *simulation*)
(defvar *golem*)
(defvar *world*)
(defclass simulation ()
  (
   ;;;; WORLD BITS
   (%next-id :initform (make-counter))
   (world :accessor world :initarg :world
	  :documentation "Our World.  The instance will be created in shared-initialize
                          using other values in our simulation.")
   (initial-creature-count
    :accessor initial-creature-count :initarg :initial-creature-count :initform 50
    :documentation
    "The world starts with this number of randomly generated tree structures" )
   
   (world-size :accessor world-size :initarg :worldsize :initform 20
	       :documentation "The size of one side of the map")
   ;;Energy drop details
   (node-energy-frequency
    :accessor node-energy-frequency :initarg :node-energy-frequency :initform .2
    :documentation "What percentage of nodes should get energy dropped on them.")
   (node-energy-max
    :accessor node-energy-max :initarg :node-energy-max :initform 70
    :documentation "(random node-energy-max) will be dropped on nodes.")
   (drop-energy-turns
    :accessor drop-energy-turns :initarg :drop-energy-turns :initform 10
    :documentation "The period between energy drops.")

   (init-creature-max-energy
    :accessor init-creature-max-energy :initarg :init-creature-max-energy :initform 128
    :documentation "The max energy of newly generated creatures")

   
   ;;;;generation specs
   (depth-bound
    :accessor depth-bound :initarg :depth-bound :initform 8
    :documentation "The depth to force rewriting termination")
   (left-branch-chance
    :accessor left-branch-chance :initarg :left-branch-chance :initform 8
    :documentation "The stochaistic amount to use when determining a random subtree
		    The weight for the left branch")
   (right-branch-chance
    :accessor right-branch-chance :initarg :right-branch-chance :initform 8
    :documentation "The stochaistic amount to use when determining a random subtree
   		    The weight for the left branch")
   (stop-chance
    :accessor stop-chance :initarg :stop-chance :initform 8
    :documentation "The stochaistic amount to use when determining a random subtree
   		    The weight for the root")

   ;;Creature Variables
   (sleep-time
    :accessor sleep-time :initarg :sleep-time :initform 2
    :documentation "How long to sleep after completing but before re-animating.")
   
   (base-mutation-rate
    :accessor base-mutation-rate :initarg :base-mutation-rate :initform .01
    :documentation "What is the default 'Should I Mutate?' rate")
   (base-value-mutation-rate
    :accessor base-value-mutation-rate :initarg :base-value-mutation-rate :initform .01
    :documentation
    "What is the default max amount of change in a value that is mutated")
   (base-mutation-depth
    :accessor base-mutation-depth :initarg :base-mutation-depth :initform 4
    :documentation "The depth bound of newly generated sub-trees")
   (default-energy-cost
    :accessor default-energy-cost :initarg :default-energy-cost  :initform 1
    :documentation "The amount of energy a function call takes")

   (beta-reduction-cost
    :initarg :beta-reduction-cost :accessor beta-reduction-cost :initform 1
    :documentation "The energy a creature uses to beta-reduce its dna")
   (function-energy-costs
    :initarg :function-energy-costs
    :reader function-energy-costs
    :initform (list (cons 'dna:move
			  (lambda (energy) (* 1.0d0 (/ energy 10))))
		    '(dna:feed . 3)
		    '(dna:energy? . 1)
		    (cons 'dna:asexually-reproduce
			  (lambda (energy) (* 1.0d0 (/ energy 2))))))
   
   (function-time-costs
    :initarg :function-time-costs
    :reader function-time-costs
    :initform '((dna:move . 10)
		(dna:feed . 3)
		(dna:energy? . 2)
		(dna:asexually-reproduce . 5)))
   
   ))

(defmethod function-time-cost ((fn-name symbol) (sim simulation))
  (cdr (assoc fn-name (function-time-costs sim) :test #'eq)))

(defmethod function-energy-cost ((fn-name symbol) (sim simulation))
  (cdr (assoc fn-name (function-energy-costs sim) :test #'eq)))

(setf *simulation* (make-instance 'simulation))

(defmethod next-id ((sim simulation))
  (funcall (slot-value sim '%next-id)))