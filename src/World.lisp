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
	  :documentation "The what to do next queue. Leftist Min Priority Queue")))

(defmethod advance-time ((world world))
  "Advance a world a tick by advancing any creatures for that tick."
  (rlogger.info "Advancing the world from tick: ~a~%" (tick-number world))
  (call-next-method) ;increment tick
  (process-queue-for-tick world)
  ;;TODO: at some point having creatures automatically recycled in here.
  )

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


(defmethod schedule (action (w world) ticks-from-now)
  (let ((tick (+ ticks-from-now (tick-number w))))
    (rlogger.dribble "Scheduling an action in the world. Tick: ~a~%" tick)
  
    (setf (queue w) (meld (queue w)
			  (make-instance 'leftist-tree-node
					 :key (+ ticks-from-now (tick-number w))
					 :data action)))))
