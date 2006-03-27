(in-package :rEvolver)
(defvar *golem*)
(defclass simulation ()
  (
   ;;;; WORLD BITS
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
    :accessor node-energy-frequency :initarg :node-energy-frequency :initform .3
    :documentation "What percentage of nodes should get energy dropped on them.")
   (node-energy-max
    :accessor node-energy-max :initarg :node-energy-max :initform 100
    :documentation "(random node-energy-max) will be dropped on nodes.")
   (drop-energy-turns
    :accessor drop-energy-turns :initarg :drop-energy-turns :initform 10
    :documentation "The period between energy drops.")

   (init-creature-max-energy
    :accessor init-creature-max-energy :initarg :init-creature-max-energy :initform 128
    :documentation "The max energy of newly generated creatures")

   
   ;;;;generation specs
   (depth-bound
    :reader depth-bound :initarg :depth-bound :initform 8
    :documentation "The depth to force rewriting termination")
   (left-branch-chance
    :reader left-branch-chance :initarg :left-branch-chance :initform 8
    :documentation "The stochaistic amount to use when determining a random subtree
		    The weight for the left branch")
   (right-branch-chance
    :reader right-branch-chance :initarg :right-branch-chance :initform 8
    :documentation "The stochaistic amount to use when determining a random subtree
   		    The weight for the left branch")
   (stop-chance
    :reader stop-chance :initarg :stop-chance :initform 8
    :documentation "The stochaistic amount to use when determining a random subtree
   		    The weight for the root")

   ;;Creature Variables
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
    :reader default-energy-cost :initarg :default-energy-cost  :initform 1
    :documentation "The amount of energy a function call takes")
   
   (function-energy-costs
    :initarg :function-energy-costs
    :reader function-energy-costs
    :initform (list (cons 'dna:move
			  (lambda (energy) (/ energy 10)))
		    '(dna:feed . 3)
		    '(dna:energy? . 1)
		    (cons 'dna:asexually-reproduce
			  (lambda (energy) (/ energy 2)))))
   
   (function-time-costs
    :initarg :function-time-costs
    :reader function-time-costs
    :initform '((dna:move . 10)
		(dna:feed . 3)
		(dna:energy? . 2)
		(dna:asexually-reproduce . 5)))
   
   ))




(defmethod (setf left-branch-chance) (val (sim simulation))
  (with-slots (left-branch-chance) sim
  (setf generator::*left-chance* (or val left-branch-chance)
	left-branch-chance (or val left-branch-chance))))

(defmethod (setf right-branch-chance) (val (sim simulation))
  (with-slots (right-branch-chance) sim
  (setf generator::*right-chance* (or val right-branch-chance)
	right-branch-chance (or val right-branch-chance))))

(defmethod (setf stop-chance) (val (sim simulation))
  (with-slots (stop-chance) sim
  (setf generator::*stop-chance* (or val stop-chance)
	stop-chance (or val stop-chance))))

(defmethod (setf depth-bound) (val (sim simulation))
  (with-slots (depth-bound) sim
  (setf generator::*depth-bound* (or val depth-bound)
	depth-bound (or val depth-bound))))

(defmethod (setf function-energy-costs) ((list list) (sim simulation))
  (with-slots (function-energy-costs) sim
    (setf function-energy-costs list))
  (clrhash *function-energy-cost-hash*)
  (mapcar
   (lambda (x)       
     (setf (gethash (car x) *function-energy-cost-hash*)
	   (cdr x)))
   list))

(defmethod (setf function-time-costs) ((list list) (sim simulation))
  (with-slots (function-time-costs) sim
    (setf function-time-costs list))
  (clrhash *function-time-cost-hash*)
  (mapcar
   (lambda (x)       
     (setf (gethash (car x) *function-time-cost-hash*)
	   (cdr x)))
   list)
  )

(defmethod random-location ((sim simulation))
  (let ((m (world-map (world sim))))
    (random-node m)))

(defmethod make-new-world ((sim simulation))
  (let ((map (make-instance '2d-array-map :x-size (world-size sim) :y-size (world-size sim))))
    (drop-random-energy map (node-energy-frequency sim) (node-energy-max sim))
    (setf (world sim) (make-instance 'world :map map))))

(defmethod populate-world ((sim simulation))
  (loop for i from 1 to (initial-creature-count sim)
	for cr = (make-instance 'Creature
				:energy (init-creature-max-energy sim)
				:world (world sim) 
				:node (random-location sim)
				:mutation-rate (base-mutation-rate sim)
				:value-mutation-rate (base-value-mutation-rate sim)
				:mutation-depth (base-mutation-depth sim)

				;;This is just attempting to test movement
				;;:dna '(dna:gamma
				;;       (dna:gamma dna:cons (dna:gamma dna:move dna:nil)) 
				;;       (dna:gamma dna:move dna:nil))
				
				)
	do
	(let ((cr cr))
;;	  (setf *golem* cr)
	  (schedule (lambda () (animate cr)) (world sim) 1))
	collect cr))


(defmethod shared-initialize :after ((sim simulation) slot-names
				     &key function-time-costs function-energy-costs depth-bound
				     left-branch-chance right-branch-chance stop-chance
				     &allow-other-keys)
  (declare (ignore slot-names))
  
  (setf (stop-chance sim) (or stop-chance (stop-chance sim)))
  (setf (right-branch-chance sim) (or right-branch-chance (right-branch-chance sim)))
  (setf (left-branch-chance sim) (or left-branch-chance (left-branch-chance sim)))
  (setf (depth-bound sim) (or depth-bound (depth-bound sim)))
  (setf (function-time-costs sim) (or function-time-costs (function-time-costs sim)))
  (setf (function-energy-costs sim) (or function-energy-costs (function-energy-costs sim)))
  
  (make-new-world sim)
  (populate-world sim)
  (labels ((drop-energy-and-readd ()
	     (drop-random-energy (world-map (world sim)) (node-energy-frequency sim) (node-energy-max sim))
	     (schedule #'drop-energy-and-readd
		       (world sim) 1)
	     ))
    ;;setup energy drops
    (schedule #'drop-energy-and-readd
	      (world sim) 1))
  )


(defparameter +simulation+ (make-instance 'simulation))
