(in-package :rEvolver)

;;(declaim (optimize (debug 3)))

(defclass creature ()
  ( 
   (unique-id :reader unique-id :initform (next-id *simulation*))
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

(defmethod clone-with-mutation ((golem creature) &key (energy #'energy) (world nil) (node nil))
  (with-slots (dna max-energy mutation-rate mutation-depth value-mutation-rate) golem
    (make-instance
     'creature
     :max-energy (maybe-mutate-value max-energy
				     mutation-rate value-mutation-rate )
     :energy (funcall energy golem) 
     :mutation-rate (maybe-mutate-value mutation-rate
					mutation-rate value-mutation-rate)
     :value-mutation-rate (maybe-mutate-value value-mutation-rate
					      mutation-rate value-mutation-rate)
     :mutation-depth (maybe-mutate-value mutation-depth
					 mutation-rate value-mutation-rate)
     
     :dna (maybe-mutate-tree (copy-tree dna) mutation-rate mutation-depth)
     :world (or world (world golem))
     :node (or node (node golem))
     )))

(defmethod alivep ((creature creature))
  (and (world creature)
       (node creature)
       (> (energy creature) 0)))

(defmethod die ((creature creature) &optional reason)
  (rlogger.debug "[~a] ~a died: ~a"
		 (tick-number (world creature))
		 creature
		 reason)
  (when (node creature)
    ;;the creature 'decays' returning energy to the system
    (when (< 0 (energy creature))
      (add-energy (node creature) (energy creature)))
    (remove-creature creature (world creature))
    (remove-creature creature (node creature)))
  (setf (energy creature) 0
	(slot-value creature 'current-continuation) nil)
  (signal 'cse:escape :reason reason)
  creature)

;(define-condition has-the-pox ( error)
;  ((original-error :initarg :original-error :accessor original-error)))

(defmethod got-the-pox ((creature creature) error)
  (die creature "Got the Pox"))

(defmethod use-energy ((creature creature) (amount (eql nil))) ())
(defmethod use-energy ((creature creature) (amount function))
  (use-energy creature (funcall amount (energy creature))))

(defmethod use-energy ((creature creature) (amount number))
  (when (< (decf (energy creature) amount)
	   (creature-minimum-energy *simulation*))
    (die creature "Died from exhaustion.")))

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

(defmethod reschedule ((creature creature) continuation ticks &optional (reason nil))
  (unless (alivep creature)
    (break "Why are you trying to reschedule a dead creature:~a?" creature))
  
  (setf (slot-value creature 'current-continuation) continuation)
  (schedule (lambda ()
	      (unless (alivep creature)
		(break "The creature died between when it was rescheduled (for: ~a) and when the continuation was called:~a " reason creature))
	      (animate creature))
	    (world creature)
	    ticks))

(defmethod animate ((creature creature))
  (unless (alivep creature)
    (rlogger.error "[~a] Apparently we tried to animate a dead creature or something. ~a"
	   (tick-number (world creature))
	   creature))
  
  (with-slots (current-continuation) creature
    (handler-bind ((cse:code-error
		    #'(lambda (er)
			(got-the-pox creature er)
			(return-from animate nil)))
		   (cse:escape
		    #'(lambda (esc) ;;the continuation should already have been set.
			(rlogger.dribble "[~a] ~a escaped because: ~a"
					 (tick-number (world creature))
					 creature
					 (cse:reason esc) )
			(return-from animate (cse:reason esc)))))
      ;;any animation costs something.
      (use-energy creature (animation-cost *simulation*))
      (incf (animation-count creature))
      (let* ((fn (or current-continuation
		    (make-interpreter (dna-of creature)
				      (creature-environment creature)
				      #'(lambda ()
					  (use-energy creature
						      (beta-reduction-cost *simulation*))))))
	     (rv (funcall fn)))
	(cond
	  ((eq 'dna:eof rv) 
	   (rlogger.dribble "[~a] ~a succesfully reached end of dna. Scheduling reanimation."
			    (tick-number (world creature))
			    creature)
	   (use-energy  creature (rerun-cost *simulation*))
	   (schedule (lambda () (animate creature))
		     (world creature)
		     (sleep-time *simulation*)))
	  
	  (T (rlogger.dribble "[~a] Creature appeared to finish, but failed to return eof: ~a"
			      (tick-number (world creature))
			      rv)
	     (got-the-pox creature "Failed to find EOF")))
	))))



(defmethod print-object ((cr creature) stream)
  (format stream "#<(Creature :Id ~a :Energy ~a :AC ~A)>"
	  (unique-id cr) (energy cr) (animation-count cr)))

