(in-package :rEvolver)

(defclass simulation ()
  (
   ;; WORLD BITS
   (world :accessor world :initarg :world
	  :documentation "Our World.  The instance will be created in shared-initialize
                          using other values in our simulation.")
   (initial-creature-count
    :accessor initial-creature-count :initarg :initial-creature-count :initform 50
    :documentation
    "The world starts with this number of randomly generated tree structures" )
   (world-size :accessor world-size :initarg :worldsize :initform 20
	       :documentation "The size of one side of the map")
   (node-energy-frequency :accessor node-energy-frequency
			  :initarg :node-energy-frequency :initform .3)
   (node-energy-max :accessor node-energy-max :initarg :node-energy-max :initform 100)
   (drop-energy-turns
    :accessor drop-energy-turns :initarg :drop-energy-turns :initform 10
    :documentation "")

   (init-creature-max-energy
    :accessor init-creature-max-energy :initarg :init-creature-max-energy :initform 128
    :documentation "The max energy of newly generated creatures")

   ;;generation specs
   (depth-bound :accessor depth-bound :initarg :depth-bound :initform 8)
   (left-branch-chance :accessor left-branch-chance :initarg :left-branch-chance :initform 8)
   (right-branch-chance :accessor left-branch-chance :initarg :left-branch-chance :initform 8)
   (stop-chance :accessor left-branch-chance :initarg :left-branch-chance :initform 8)

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
   
   ))

(defmethod shared-initialize :after ((sim simulation) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (make-new-world sim)
  (populate-world sim)
  )

(defmethod random-location ((sim simulation))
  (let ((m (world-map (world sim))))
    (random-node m)))

(defmethod make-new-world ((sim simulation))
  (let ((map (make-instance '2d-array-map :x-size (world-size sim) :y-size (world-size sim))))
    (drop-random-energy map (node-energy-frequency sim) (node-energy-max sim))
    (setf (world sim) (make-instance 'world :map map))))

(defmethod populate-world ((sim simulation))
  (loop for i to (initial-creature-count sim)
	for cr = (make-instance 'Creature
				:energy (init-creature-max-energy sim)
				:world (world sim) 
				:node (random-location sim)
				:mutation-rate (base-mutation-rate sim)
				:value-mutation-rate (base-value-mutation-rate sim)
				:mutation-depth (base-mutation-depth sim))
	do
	(let ((cr cr))
	  (schedule (lambda () (animate cr)) (world sim) 1))
	collect cr))

(defparameter +simulation+ (make-instance 'simulation))
