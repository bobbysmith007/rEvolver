(in-package :rEvolver)

;;(declaim (optimize (debug 3)))

(defclass creature ()
  (
   (mutation-depth :accessor mutation-depth :initarg :mutation-depth
		   :initform (base-mutation-depth *simulation*))
   (mutation-rate :accessor mutation-rate :initarg :mutation-rate
		  :initform (base-mutation-rate *simulation*))
   (value-mutation-rate :accessor value-mutation-rate :initarg :value-mutation-rate
			:initform (base-value-mutation-rate *simulation*) )
   (max-energy :accessor max-energy :initform 0 :initarg :max-energy)
   
   (energy :accessor energy :initform 0 :initarg :energy)
   (node :accessor node )
   (world :reader world :initarg :world)
   (dna :accessor dna-of :initarg :dna :initform (generate-tree (depth-bound *simulation*)))
   (current-continuation :initform nil)
   (animation-count :accessor animation-count :initform 0)
   ))
(defmethod initialize-instance :after ((creature creature) &rest slots
				       &key node world max-energy energy
				       &allow-other-keys)
  (declare (ignore slots))
  (add-creature creature node)
  (add-creature creature world)

  (if (not max-energy)
      (setf (max-energy creature)
	    (or energy (energy creature))))
  )


(defgeneric asexually-reproduce (golem))
(defgeneric clone-with-mutation (golem))

(defmethod clone-with-mutation ((golem creature))
  (with-slots (dna max-energy mutation-rate mutation-depth value-mutation-rate) golem
    (make-instance
     'creature
     :max-energy (maybe-mutate-value max-energy
				     mutation-rate value-mutation-rate )
     :energy (energy golem) 
     :mutation-rate (maybe-mutate-value mutation-rate
					mutation-rate value-mutation-rate)
     :value-mutation-rate (maybe-mutate-value value-mutation-rate
					      mutation-rate value-mutation-rate)
     :mutation-depth (maybe-mutate-value mutation-depth
					 mutation-rate value-mutation-rate)
     
     :dna (maybe-mutate-tree (copy-tree dna) mutation-rate mutation-depth)
     :world (world golem)
     :node (node golem)
     )))

(defmethod alivep ((creature creature))
  (and (world creature)
       (node creature)
       (> (energy creature) 0)))

(defmethod die ((creature creature))
  (rlogger.info "[~a] Creature died: ~a"
		(tick-number (world creature))
		creature)
  (when (node creature)
    ;;the creature 'decays' returning energy to the system
    (when (< 0 (energy creature))
      (add-energy (node creature) (energy creature)))
    (remove-creature creature (world creature))
    (remove-creature creature (node creature)))
  (setf (energy creature) 0
	(slot-value creature 'current-continuation) nil)
  creature)

;(define-condition has-the-pox ( error)
;  ((original-error :initarg :original-error :accessor original-error)))

(defmethod got-the-pox ((creature creature) (error error))
  (rlogger.dribble "[~a] ~a got the pox: ~a"
		   (tick-number (world creature))
		   creature
		   error)
  (die creature)
					;(escape (slot-value error 'original-error))
  )

(defmethod use-energy ((creature creature) (amount (eql nil))) ())
(defmethod use-energy ((creature creature) (amount function))
  (use-energy creature (funcall amount (energy creature))))

(defmethod use-energy ((creature creature) (amount number))
  (when (>= 0 (decf (energy creature) amount))
    (die creature)
    (signal 'cse:escape :reason 'died-from-exhaustion)))

(defmethod add-creature ((creature creature) (node node))
  "add a creature to a node."
  (push creature (creatures-of node))
  (setf (node creature) node))

(defmethod remove-creature ((creature creature) (node node))
  "Take a creature out of a node."
  (setf (creatures-of node) (delete creature (creatures-of node) :test #'eq))
  (setf (node creature) nil))

(defmethod add-creature ((golem creature) (terra world))
  (with-slots (creature-count) terra
    (setf creature-count (1+ creature-count)))
  )
(defmethod remove-creature ((golem creature) (terra world))
  (with-slots (creature-count) terra
    (setf creature-count (1- creature-count)))
  )

(defmethod reschedule ((creature creature) continuation ticks)
  (setf (slot-value creature 'current-continuation) continuation)
  (schedule (lambda () (animate creature)) (world creature) ticks))

(defmethod animate ((creature creature))
  (handler-bind ((cse:code-error
		  #'(lambda (er)
		      (got-the-pox creature er)
		      (return-from animate nil)))
		 (cse:escape
		  #'(lambda (esc)
		      (return-from animate (cse:reason esc)))))
    (with-slots (current-continuation) creature
      
      (let ((rv
	     (cond
	       (current-continuation
		(incf (animation-count creature))
		(rlogger.dribble "Continuing previously suspended creature: ~a"
				 current-continuation)
		(funcall current-continuation))
	       ((alivep creature)
		(incf (animation-count creature))
		(rlogger.dribble "Starting the creature anew.")
		(funcall (make-interpreter (dna-of creature)
					   (creature-environment creature)
					   #'(lambda ()
					       (use-energy creature (beta-reduction-cost *simulation*)))))))))

	(if rv
	    (rlogger.dribble "[~a] Creature animated successfully: ~a"
			     (tick-number (world creature))
			     rv)
	    (rlogger.dribble "[~a] Creature is dead or something."))
	))))



(defmethod print-object ((cr creature) stream)
  (format stream "#<(Creature :Energy ~a AC:~A)>" (energy cr) (animation-count cr)))

