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
  (format T "Advancing the world from tick: ~a~%" (tick-number world))
  (with-accessors ((current-tick tick-number) (queue queue)) world
		  ;;if the next element in the queue is for this tick,
		  ;; then take it off the front and process each creature
		  (unless (null queue)
		    (loop
			for x = (key queue)
			while (= current-tick x) 
			do
			(multiple-value-bind (current-node new-queue)
			    (pop-tree! (queue world))
			  (setf (queue world) new-queue)
			  (let ((time (key current-node))
				(action (data current-node)))
			    (execute-1 (time action)))
			  ))))
  )


(defmethod schedule (action (w world) ticks-from-now)
  (format-log "Scheduling an action in the world. Tick: ~a~%" (+ ticks-from-now (tick-number w)))
  (setf (queue w) (schedule action (queue w) (+ ticks-from-now (tick-number w)))))