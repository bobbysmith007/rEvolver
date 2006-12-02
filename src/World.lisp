(in-package :rEvolver)

(defclass world (ticker)
  ((revolver-map :initarg :map
	      :accessor revolver-map)

   (queue :initform (make-instance 'pairing-heap :buffer-size 0)
	  :accessor queue
	  :documentation "The what to do next queue. Leftist Min Priority Queue")
      
   (population-infusions :initform 0 :accessor population-infusions)
   (repopulation-infusions :initform 0 :accessor repopulation-infusions)
   ))
(defmethod process-queue-for-tick ((world world))
  (with-accessors ((tick tick-number) (queue queue)) world
		  ;;if the next element in the queue is for this tick,
		  ;; then take it off the front and process each creature
 
		  (loop
		      while (and (not (empty-p queue))
				 (= tick
				    (key (peek queue))))
		    for (key data) = (multiple-value-list (dequeue queue))
		    do
		    (restart-case (funcall data)
				  (continue-next-action () nil)))))

(defgeneric schedule (action world ticks-from-now))

(defmethod schedule (action (w world) ticks-from-now)
  (insert (queue w)
	  (+ ticks-from-now (tick-number w))
	  action))

(defmethod random-location ((w world))
  (let ((m (revolver-map w)))
    (random-node m)))

(defmethod populate-world ((w World) num)
  (incf (population-infusions w) num)
  (rlogger.info "[~a] Populating world with: ~a"
		(tick-number w)
		num)
 
	(loop repeat num
	  do (let ((cr (make-instance 'Creature
				      :energy (init-creature-max-energy *simulation*)
				      :world w 
				      :node (random-location w)
				      :mutation-rate (base-mutation-rate *simulation*)
				      :value-mutation-rate (base-value-mutation-rate *simulation*)
				      :mutation-depth (base-mutation-depth *simulation*)
				      
				      ;;DNA to move twice
				      ;;:dna '(dna:gamma (dna:gamma dna:cons (dna:gamma dna:move nil)) (dna:gamma dna:move nil))
				      )))
	       (schedule #'(lambda ()
			     (animate cr (creature-fn cr))) w 1))
	  ))

(defmethod creatures ((w world))
  (creatures (revolver-map w)))

(defmethod repopulate-world ((w World) num)
  (rlogger.info "[~a] Repopulating world with: ~a"
		(tick-number w)
		num)
  (incf (repopulation-infusions w) num)
  (let* ((cl (creatures w))
	 (creatures (make-array (length cl) :initial-contents cl))
	 )
    
    (loop for i to num
	  for cr = (random-elt creatures)
	  for new-cr = (clone-with-mutation cr
					    :energy #'max-energy
					    :node (random-node (revolver-map *world*)))
	  do
	  (let ((new-cr new-cr))
	    
	    
	    (schedule (lambda () (animate new-cr (creature-fn new-cr))) w 1))
	  collect new-cr)))

(defmethod advance-time :around ((world world))
  (let ((the-tick (tick-number world))
	(node-count (* (world-size *simulation*)
		      (world-size *simulation*))))
 
    (rlogger.info "[~a] Creatures: ~a (~a/node)  Animation-record: ~a  population(~a,~a) free-energy: ~a (~a/node)"
		 
		   the-tick
		   (creature-count (revolver-map *world*))
		   (float (/ (creature-count (revolver-map *world*))
			     node-count))
		   (if *golem* (animation-count *golem*) 0)
		   (repopulation-infusions *world*)
		   (population-infusions *world*)
		   (free-energy (revolver-map *world*))
		   (truncate
		    (/ (free-energy (revolver-map *world*))
		       node-count)))
    (call-next-method)
;    (rlogger.info "Finished  tick: ~a creature-count: ~a free-energy: ~a (~a/node)"
;		  the-tick
;		  (creature-count (revolver-map world))
;		  (free-energy (revolver-map world))
;		  (truncate
;		   (/ (free-energy (revolver-map world))
;		      (* (world-size *simulation*)
;			 (world-size *simulation*)))))
    ))

(defmethod advance-time ((world world))
  "Advance a world a tick by advancing any creatures for that tick."
  
  (when (<= (creature-count (revolver-map world))
	    (* (initial-creature-count *simulation*) .10))
    ;;The point here is to reward the most rugged creatures by using them
    ;;as a base for the next generation
    (when (> (creature-count (revolver-map world)) 0)
      (repopulate-world world
			(truncate (* .2 (- (initial-creature-count *simulation*)
					   (creature-count (revolver-map world)))))))
    ;;as well as some fresh ones.
    (populate-world world (- (initial-creature-count *simulation*)
			     (creature-count (revolver-map world)))))
  
  (call-next-method) ;increment tick
  (process-queue-for-tick world)
  ;;TODO: at some point having creatures automatically recycled in here.
  )

(defun make-new-world (&key init-creature-count)
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
	(schedule #'drop-energy-and-re-add world 1)
	(populate-world world (or init-creature-count (initial-creature-count *simulation*))))
      world)))

(defmethod drop-random-energy ((world world) frequency energy-to-add-max/spot )
  (let ((m (revolver-map world))
	(sum 0))
    (dotimes (n (* frequency (x-size m) (y-size m)))
      (let* ((l (random-node m))
	     (e (random energy-to-add-max/spot)))
	(incf sum (add-energy l e))))
    (rlogger.info "[~a] Dropped Energy: ~a"
			       (tick-number world)
			       sum)
    sum))
