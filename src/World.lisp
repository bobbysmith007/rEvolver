(in-package :rEvolver)

(defclass world (ticker)
  ((possible-creatures :initform '()
		       :allocation :class
		       :accessor possible-creatures
		       :documentation "List of classes of creatures that are allowed in this world")
   (world-map :initarg :map
	      :accessor world-map)

   (queue :initform '()
	  :accessor queue
	  :documentation "The what to do next queue. Leftist Min Priority Queue")
   (creature-count :initform 0 :accessor creature-count)
   (unconsumed-energy-in-the-world :initform 0 :accessor unconsumed-energy-in-the-world)
   (population-infusions :initform 0 :accessor population-infusions)
   ))
(defmethod process-queue-for-tick ((world world))
  (with-accessors ((tick tick-number) (queue queue)) world
		  ;;if the next element in the queue is for this tick,
		  ;; then take it off the front and process each creature
 
		  (loop
		    while (and queue
			       (= tick
				  (key queue))) 
		    do
		    (multiple-value-bind (current-node new-queue)
			(pop-tree! (queue world))
		      (setf queue new-queue)
		      (rlogger.dribble "[~a] Calling scheduled action: ~a"
				       tick
				       (data current-node))
		      (restart-case (funcall (data current-node))
		       (continue-next-action () nil))))))

(defgeneric schedule (action world ticks-from-now))

(defmethod schedule (action (w world) ticks-from-now)
  (let ((tick (+ ticks-from-now (tick-number w))))
    (rlogger.dribble "[~a] Scheduling an action in the world: ~a." (tick-number w) tick)
  
    (setf (queue w) (meld (queue w)
			  (make-instance 'leftist-tree-node
					 :key (+ ticks-from-now (tick-number w))
					 :data action)))))

(defmethod random-location ((w world))
  (let ((m (world-map w)))
    (random-node m)))

(defmethod populate-world ((w World))
  (loop for i from 1 to (initial-creature-count *simulation*)
	for cr = (make-instance 'Creature
				:energy (init-creature-max-energy *simulation*)
				:world w 
				:node (random-location w)
				:mutation-rate (base-mutation-rate *simulation*)
				:value-mutation-rate (base-value-mutation-rate *simulation*)
				:mutation-depth (base-mutation-depth *simulation*)

				;;:dna '(dna:gamma (dna:gamma dna:cons (dna:gamma dna:move nil)) (dna:gamma dna:move nil))
				)
	do
	(let ((cr cr))
;;	  (setf *golem* cr)
	  (schedule (lambda () (animate cr)) w 1))
	collect cr))

(defmethod advance-time ((world world))
  "Advance a world a tick by advancing any creatures for that tick."
  (rlogger.info "Advancing the world from tick: ~a~%" (tick-number world))
  (when (= (creature-count world) 0)
    (incf (population-infusions world))
    (populate-world world))
  (call-next-method) ;increment tick
  (process-queue-for-tick world)
  ;;TODO: at some point having creatures automatically recycled in here.
  )

(defmethod add-energy ((world world) value)
  (incf (unconsumed-energy-in-the-world world) (* 1.0d0 value)))

(defmethod remove-energy ((world world) value)
  (decf (unconsumed-energy-in-the-world world) (* 1.0d0 value)))

(defun make-new-world ()
  (let ((map (make-instance '2d-array-map
			    :x-size (world-size *simulation*)
			    :y-size (world-size *simulation*))))
    
    (let ((world (make-instance 'world :map map)))
      (drop-random-energy world
			  (node-energy-frequency *simulation*)
			  (node-energy-max *simulation*))
      (labels ((drop-energy-and-re-add ()
		 (drop-random-energy world
				     (node-energy-frequency *simulation*)
				     (node-energy-max *simulation*))
		 (schedule #'drop-energy-and-re-add world
			   (drop-energy-turns *simulation*))
		 ))
	;;setup energy drops
	(schedule #'drop-energy-and-re-add world 1))
      world)))

(defmethod drop-random-energy ((world world) frequency energy-to-add-max/spot )
  (let ((m (world-map world))
	(sum 0))
    (dotimes (n (* frequency (x-size m) (y-size m)))
      (let* ((l (random-node m))
	     (e (random energy-to-add-max/spot))
	     (actual-energy (add-energy l e)))
	
	(incf sum actual-energy)
	(add-energy world actual-energy)))
    sum))