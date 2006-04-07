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
    :accessor initial-creature-count :initarg :initial-creature-count :initform 640
    :documentation
    "The world starts with this number of randomly generated tree structures" )
   
   (world-size :accessor world-size :initarg :worldsize :initform 32
	       :documentation "The size of one side of the map")
   ;;Energy drop details
   (node-energy-frequency
    :accessor node-energy-frequency :initarg :node-energy-frequency :initform .2
    :documentation "What percentage of nodes should get energy dropped on them.")
   (node-energy-max
    :accessor node-energy-max :initarg :node-energy-max :initform 32
    :documentation "(random node-energy-max) will be dropped on nodes.")
   (drop-energy-turns
    :accessor drop-energy-turns :initarg :drop-energy-turns :initform 16
    :documentation "The period between energy drops.")

   
   
   ;;;;generation specs
   (depth-bound
    :accessor depth-bound :initarg :depth-bound :initform 12
    :documentation "The depth to force rewriting termination")
   (left-branch-chance
    :accessor left-branch-chance :initarg :left-branch-chance :initform 10
    :documentation "The stochaistic amount to use when determining a random subtree
		    The weight for the left branch")
   (right-branch-chance
    :accessor right-branch-chance :initarg :right-branch-chance :initform 10
    :documentation "The stochaistic amount to use when determining a random subtree
   		    The weight for the left branch")
   (stop-chance
    :accessor stop-chance :initarg :stop-chance :initform 8
    :documentation "The stochaistic amount to use when determining a random subtree
   		    The weight for the root")

   ;;Creature Variables
   (creature-minimum-energy
    :accessor creature-minimum-energy
    :initarg :creature-minimum-energy
    :initform 32
    :documentation "A lot of the costs are fractions of the current energy.
We want to have a non-zero minimum so they can die from these functions.")
   
   (init-creature-max-energy
    :accessor init-creature-max-energy :initarg :init-creature-max-energy :initform 256
    :documentation "The max energy of newly generated creatures")
   
   (sleep-time
    :accessor sleep-time :initarg :sleep-time :initform 2
    :documentation "How long to sleep after completing but before re-animating.")
   (rerun-cost
    :accessor rerun-cost :initarg :rerun-cost
    :initform (lambda (val)  (truncate (1+ (/ val 10))))
    :documentation "The amount of energy that rerunning the program takes")
   
   (base-mutation-rate
    :accessor base-mutation-rate :initarg :base-mutation-rate :initform .05
    :documentation "What is the default 'Should I Mutate?' rate")
   (base-value-mutation-rate
    :accessor base-value-mutation-rate :initarg :base-value-mutation-rate :initform .05
    :documentation
    "What is the default max amount of change in a value that is mutated")
   (base-mutation-depth
    :accessor base-mutation-depth :initarg :base-mutation-depth :initform 6
    :documentation "The depth bound of newly generated sub-trees")

   (animation-cost
    :accessor animation-cost :initarg :animation-cost :initform 4
    :documentation "How much energy any call to animate should cost. This is another failsafe.")
   (beta-reduction-cost
    :initarg :beta-reduction-cost :accessor beta-reduction-cost :initform 2
    :documentation "The energy a creature uses to beta-reduce its dna")
   (function-energy-costs
    :initarg :function-energy-costs
    :reader function-energy-costs
    :initform (list (cons 'dna:move
			  (lambda (energy) (truncate (/ energy 8))))
		    '(dna:feed . 16)
		    '(dna:energy? . 4)
		    (cons 'dna:asexually-reproduce
			  ;;each creature get's half the (original- min)
			  ;; lost due to entropy
			  (lambda (energy)
			    (truncate (/ (+ energy
					    (creature-minimum-energy *simulation*))
					 2))))))
   
   (function-time-costs
    :initarg :function-time-costs
    :reader function-time-costs
    :initform '((dna:move . 4)
		(dna:feed . 3)
		(dna:energy? . 2)
		(dna:asexually-reproduce . 6)))
   
   ))

(defmethod function-time-cost ((fn-name symbol) (sim simulation))
  (cdr (assoc fn-name (function-time-costs sim) :test #'eq)))

(defmethod function-energy-cost ((fn-name symbol) (sim simulation))
  (cdr (assoc fn-name (function-energy-costs sim) :test #'eq)))

(setf *simulation* (make-instance 'simulation))

(defmethod next-id ((sim simulation))
  (funcall (slot-value sim '%next-id)))