(in-package :rEvolver)

(declaim (optimize (debug 3)))

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
   (node :accessor node :initform nil)
   (world :reader world :initarg :world)
   (dna :accessor dna-of :initarg :dna :initform (generate-tree (depth-bound *simulation*)))
   (creature-fn :accessor creature-fn :initform nil :initarg creature-fn)
   (animation-count :accessor animation-count :initform 0)
   ))
(defmethod initialize-instance :after ((creature creature) &rest slots
				       &key node  max-energy energy creature-fn
				       &allow-other-keys)
  (declare (ignore slots))
  (add-creature creature node)
  
  (if (not max-energy)
      (setf (max-energy creature)
	    (or energy (energy creature))))
  
    (setf (creature-fn creature)
	  (or creature-fn
	      (make-interpreter (dna-of creature)
				(creature-environment creature)
				#'(lambda ()
				    (use-energy creature
						(beta-reduction-cost *simulation*))))))
  )


(defgeneric asexually-reproduce (golem))

(defmethod clone-with-mutation ((golem creature)
				&key (energy #'energy) (world nil) (node nil))
  (with-slots (dna max-energy mutation-rate mutation-depth
		   value-mutation-rate creature-fn)
      golem
    (multiple-value-bind (new-dna dna-changed)
	(maybe-mutate-tree dna mutation-rate mutation-depth)
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
       
       :dna new-dna
       :creature-fn (unless dna-changed creature-fn)
       :world (or world (world golem))
       :node (or node (node golem))
       ))))

(defmethod alivep ((creature creature))
  (and (world creature)
       (node creature)
       (> (energy creature) 0)))

(defmethod die ((creature creature) &optional reason)
  (rlogger.debug "[~a] ~a died: ~a"
		 (tick-number (world creature))
		 creature
		 reason)
  (when (or (null *golem*)
	    (> (animation-count creature)
	       (animation-count *golem*)))
    (rlogger.error "[~a] New animation-count record: ~a"
		  (tick-number (world creature))
		  (animation-count creature))
    (setf *golem* creature))
  (unless (node creature)
    ;;we need to be able to remove them to keep correct counts.
    ;;but maybe they already have been.
    (break "Creature died while not on a node."))
  (when (node creature)
    ;;the creature 'decays' returning energy to the system
    (when (< 0 (energy creature))
      (add-energy (node creature) (energy creature)))
    (remove-creature creature (node creature)))
  (setf (energy creature) 0)
  (signal 'cse:escape :reason reason)
  creature)


(defmethod use-energy ((creature creature) (amount (eql nil))) ())
(defmethod use-energy ((creature creature) (amount function))
  (use-energy creature (truncate (funcall amount (energy creature)))))

(defmethod use-energy ((creature creature) (amount number))
  (when (< (decf (energy creature) amount)
	   (creature-minimum-energy *simulation*))
    (die creature "Died from exhaustion.")))

(defmethod add-creature ((creature creature) (node node))
  "add a creature to a node."
  (if (node creature)
      (error "Creature is already on a node.")
      (progn (push creature (creatures-of node))
	     (setf (node creature) node)
	     (incf (creature-count (revolver-map node)))))
  )

(defmethod remove-creature ((creature creature) (node node))
  "Take a creature out of a node."
  (if (null (node creature))
      (error "Can't remove a creature that isn't on a node.")
      (progn
	(setf (creatures-of node) (delete creature (creatures-of node))
		   (node creature) nil)
	(decf (creature-count (revolver-map node))))))


(defmethod reschedule ((creature creature) continuation ticks &optional (reason nil))
  (unless (alivep creature)
    (break "Why are you trying to reschedule a dead creature:~a?" creature))
  
  
  (schedule (lambda ()
	      "The rescheduling lambda"
	      (unless (alivep creature)
		(break "The creature died between when it was rescheduled (for: ~a) and when the continuation was called:~a "
		       reason creature))
	      (animate creature continuation))
	    (world creature)
	    ticks))

(defmethod animate ((creature creature) (cr-fn function))
  (unless (alivep creature)
    (rlogger.error "[~a] Apparently we tried to animate a dead creature or something. ~a"
		   (tick-number (world creature))
		   creature))
  
  (handler-bind ((cse:escape
		  #'(lambda (esc);;the continuation should already have been set.
		      (rlogger.dribble "[~a] ~a escaped because: ~a"
				       (tick-number (world creature))
				       creature
				       (cse:reason esc) )
		      (return-from animate (cse:reason esc)))))
    (handler-bind ((cse:code-error
		    #'(lambda (er)
			(die creature (cse::original-error er))
			(break "after the pox")
			(return-from animate nil))))
      ;;any animation costs something.
      (use-energy creature (animation-cost *simulation*))
      (incf (animation-count creature))
      
      (let ((rv (funcall cr-fn)))
	(cond
	  ((eq 'dna:eof rv)
	   (rlogger.dribble "[~a] ~a succesfully reached end of dna. Scheduling reanimation."
			    (tick-number (world creature))
			    creature)
	   (use-energy creature (rerun-cost *simulation*))
	   (reschedule creature (creature-fn creature) (sleep-time *simulation*) 'dna:eof))
	    
	  (T (rlogger.error "[~a] Creature appeared to finish, but failed to return eof: ~a"
			    (tick-number (world creature))
			    rv)
	     (got-the-pox creature "Failed to find EOF")))))))



(defmethod print-object ((cr creature) stream)
  (format stream "#<(Creature :Id ~a :Energy ~a :AC ~A)>"
	  (unique-id cr) (energy cr) (animation-count cr)))

(defun find-creature (id world)
  (some #'identity (map-nodes (lambda (node)
				(find id
				      (creatures-of node)
				      :key #'unique-id))
			      (revolver-map world))))
