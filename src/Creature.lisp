(in-package :rEvolver)

(declaim (optimize (debug 3)))

(defclass creature ()
  (
   (mutation-rate :accessor mutation-rate :initarg :mutation-rate :initform generator::*mutation-rate*)
   (value-mutation-rate :accessor value-mutation-rate :initarg :value-mutation-rate :initform generator::*value-mutation-rate* )
   (init-energy :accessor init-energy :initform 0 :initarg :energy)
   
   (energy :accessor energy :initform 0 :initarg :energy)
   (node :accessor node )
   (world :reader world :initarg :world)
   (dna :accessor dna-of :initarg :dna :initform (generate-tree 3))
   (current-continuation :initform nil)
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
  creature)

(define-condition has-the-pox ( error)
  ((original-error :initarg :original-error)))

(defmethod got-the-pox ((creature creature) (error error))
  (rlogger.dribble "[~a] ~a got the pox: ~a"
		   (tick-number (world creature))
		   creature
		   error)
  (die creature)
  (escape (slot-value error 'original-error)))

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
  (handler-bind ((has-the-pox
		  (lambda (er) (got-the-pox creature er))))
    (with-slots (current-continuation) creature
      (let ((rv
	     (cond
	       (current-continuation
		(rlogger.dribble "Continuing previously suspended creature: ~a"
				 current-continuation)
		(funcall current-continuation))
	       (t (rlogger.dribble "Starting the creature anew.")
		  (funcall (make-interpreter (dna-of creature)
					     (creature-environment creature)))))))
	(rlogger.dribble "[~a] Creature animated successfully: ~a"
			 (tick-number (world creature))
			 rv)
	))))



(defmethod print-object ((cr creature) stream)
  (format stream "#<(Creature :Energy ~a)>" (energy cr)))

(defmethod asexually-reproduce ((golem creature))
  (with-slots (init-energy mutation-rate value-mutation-rate dna)
      golem
    (make-instance 'creature
		   :energy (maybe-mutate-value init-energy
					       mutation-rate value-mutation-rate )
		   :mutation-rate (maybe-mutate-value mutation-rate
						      mutation-rate value-mutation-rate)
		   :value-mutation-rate (maybe-mutate-value value-mutation-rate
							    mutation-rate value-mutation-rate)
		   
		   :dna (maybe-mutate-tree (copy-tree dna) mutation-rate)
		   :world (world golem)
		   :node (node golem)
		   )))


