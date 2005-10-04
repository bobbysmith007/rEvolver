(in-package :rEvolver)

(defclass world (ticker)
  ((possible-creatures :initform '()
		       :allocation :class
		       :accessor possible-creatures
		       :documentation "List of classes of creatures that are allowed in this world")
   (world-map :initarg :map
	      :accessor world-map)
   (queue :initform '() :accessor queue :documentation "The what to do next queue.")))

(defmethod advance-time (world)
  "Advance a world a tick by advancing any creatures for that tick."
  (with-accessors ((current-tick tick-number) (queue queue)) world
		  ;;if the next element in the queue is for this tick,
		  ;; then take it off the front and process each creature
		  (when (eq (tick-number (first queue)) current-tick)
		    (dolist (action (actions (pop queue)))
		      (funcall action))))
  (call-next-method))
