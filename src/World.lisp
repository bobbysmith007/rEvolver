(in-package :rEvolver)

(defclass world (ticker)
  ((possible-creatures :initform '()
		       :allocation :class
		       :accessor possible-creatures
		       :documentation "List of classes of creatures that are allowed in this world")
   (world-map :initarg :map
	      :accessor world-map)
   (queue :initform '() :accessor queue :documentation "The what to do next queue.")))

(defmethod advance-time ((world world))
  "Advance a world a tick by advancing any creatures for that tick."
  (format-log "Advancing the world from tick: ~a~%" (tick-number world))
  (with-accessors ((current-tick tick-number) (queue queue)) world
		  ;;if the next element in the queue is for this tick,
		  ;; then take it off the front and process each creature
		  (when (and queue (= (tick-number (first queue)) current-tick))
		    (execute (pop queue))))
  (call-next-method))


(defmethod schedule (action (w world) ticks-from-now)
  (format-log "Scheduling an action in the world. Tick: ~a~%" (+ ticks-from-now (tick-number w)))
  (setf (queue w) (schedule action (queue w) (+ ticks-from-now (tick-number w)))))