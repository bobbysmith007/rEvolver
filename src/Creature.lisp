(in-package :rEvolver)

(declaim (optimize (debug 3)))

(defclass creature ()
  (
   (mutation-depth :accessor mutation-depth :initarg :mutation-depth :initform generator::*mutation-depth*)
   (mutation-rate :accessor mutation-rate :initarg :mutation-rate :initform generator::*mutation-rate*)
   (value-mutation-rate :accessor value-mutation-rate :initarg :value-mutation-rate :initform generator::*value-mutation-rate* )
   (init-energy :accessor init-energy :initform 0 :initarg :energy)
   
   (energy :accessor energy :initform 0 :initarg :energy)
   (node :accessor node )
   (world :reader world :initarg :world)
   (dna :accessor dna-of :initarg :dna :initform (generate-tree 3))
   (current-continuation :initform nil)
   (animation-count :accessor animation-count :initform 0)
   ))

(defmethod initialize-instance :after ((creature creature) &rest slots
				       &key node
				       &allow-other-keys)
  (declare (ignore slots))
  (add-creature creature node))


(defmethod alivep ((creature creature))
  (> (energy creature) 0))

(defmethod die ((creature creature))
  (rlogger.info "[~a] Creature died: ~a"
		  (tick-number (world creature))
		  creature)
  (when (node creature)
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

(defmethod use-energy ((creature creature) amount)
  (when (>= 0 (decf (energy creature) amount))
    (signal 'dead :creature creature)))

(defmethod add-creature ((creature creature) (node node))
  "add a creature to a node."
  (push creature (creatures-of node))
  (setf (node creature) node))

(defmethod remove-creature ((creature creature) (node node))
  "Take a creature out of a node."
  (setf (creatures-of node) (delete creature (creatures-of node) :test #'eq))
  (setf (node creature) nil))

(defmethod suspend ((creature creature) continuation ticks)
  (setf (slot-value creature 'current-continuation) continuation)
  (schedule (lambda () (animate creature)) (world creature) ticks))

(defmethod animate ((creature creature))
  (handler-bind ((cse:code-error
		  (lambda (er)
		    (got-the-pox creature er)
		    (return-from animate nil))))
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
					     (creature-environment creature)))))))

	(if rv
	    (rlogger.dribble "[~a] Creature animated successfully: ~a"
			     (tick-number (world creature))
			     rv)
	    (rlogger.dribble "[~a] Creature is dead or something."))
	))))



(defmethod print-object ((cr creature) stream)
  (format stream "#<(Creature :Energy ~a AC:~A)>" (energy cr) (animation-count cr)))

(defmethod asexually-reproduce ((golem creature))
  (rlogger.info "WOOHOO! Reproduction! ~a " golem)
  (with-slots (init-energy mutation-rate value-mutation-rate dna mutation-depth)
      golem
    (let ((cr (make-instance 'creature
		   :energy (maybe-mutate-value init-energy
					       mutation-rate value-mutation-rate )
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
      (schedule #'(lambda ()
		    (rlogger.dribble "We are about to animate a NEW CREATURE! ~a" cr)
		    (animate cr)) (world golem) +reproduction-time+))))


