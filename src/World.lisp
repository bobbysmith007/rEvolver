(in-package :rEvolver)

(defclass tick-list (ticker)
  ((creature-list :initform '()
		  :accessor creatures
		  :documentation "The list of creatures that should be run at the given tick.")))
(defmethod initialize-instance :after ((tl tick-list) &key a-creature)
  (push a-creature tl))

(defmethod push-tl (creature (tl tick-list))
  "Push a creature onto a tick-list and return the tick-list"
  (with-accessors ((creatures creatures)) tl
		  (push creature creatures))
  tl)


(defclass world (ticker)
  ((possible-creatures :initform '()
		       :allocation :class
		       :accessor possible-creatures
		       :documentation "List of classes of creatures that are allowed in this world")
   (world-map :initarg :map
	      :accessor world-map)
   (queue :initform '() :accessor queue :documentation "The who comes next queue.")))

(defmethod advance-time (world)
  "Advance a world a tick by advancing any creatures for that tick."
  (with-accessors ((current-tick tick-number) (queue queue)) world
		  ;;if the next element in the queue is for this tick,
		  ;; then take it off the front and process each creature
		  (when (eq (tick-number (first queue)) current-tick)
		    (dolist (creature (creatures (pop queue)))
		      (advance-time creature))))
  (call-next-method))


(defmethod schedule-turn ((cr creature) (world world) ticks)
  (let ((tick-number-to-insert (+ ticks (tick-number world))))
    (flet ((scan (q)
	     (symbol-macrolet ((test-tick-list (first q)))
	       (cond ((eq tick-number-to-insert (tick-number test-tick-list))
		      (push-tl cr test-tick-list))
		     ((> tick-number-to-insert (tick-number test-tick-list))
		      (scan (rest q)))
		     (T
		      (push (make-instance 'tick-list :tick-number tick-number-to-insert :a-creature cr) q)))))
	   (scan (queue w))))))




;;Not following this route right now.
;(defmethod initialize-instance :after (world &key initial-creature-count)
;  ;;If they didn't specify how many creatures to start the world with, then use the default
;  (let ((initial-creature-count (or initial-creature-count +initial-creature-count+)))
;    (loop repeat initial-creature-count do
;      (add-random-creature world))))

;(defgeneric add-random-creature (world)
;  (:documentation "Create a random creature and add it to the world."))

;(defmethod add-random-creature (world)
;   )
  

