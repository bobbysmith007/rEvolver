(in-package :rEvolver)

(defparameter +movement-energy-ratio+ 1/10)
(defparameter +movement-time+ 10)

(defclass creature ()
  ((energy :accessor energy :initform 0 :initarg :energy)
   (node :accessor node :initarg :node)
   (world :reader world :initarg :world)
   dna
   current-machine-state
   ))

(defmethod die ((creature creature))
  (rlogger.info "[~a] Creature died: ~a"
		  (tick-number (world creature))
		  creature)
  (when (node creature)
    (remove-creature creature (node creature)))
  creature)

(defmethod use-energy ((creature creature) amount)
  (when (>= 0 (decf (energy creature) amount))
    (signal 'dead :creature creature)))

(defmethod add-creature ((creature creature) (node node))
  "add a creature to a node."
  (push creature (creatures node))
  (setf (node creature) node))

(defmethod remove-creature ((creature creature) (node node))
  "Take a creature out of a node."
  (delete creature (creatures node) :test #'eq)
  (setf (node creature) nil))


(defmethod move ((creature creature) &optional node)
  "Move the creature."
  ;get a new (unique) lexical binding for the creature.
  ;we need to be able to lexically close on creature.
  
  ;;before we move them to the new node use the energy (which might kill them)
  (use-energy creature (* (energy creature) +movement-energy-ratio+))  
  
  (remove-creature creature (node creature))
  
  ;;if the creature didn't specify then pick a random direction.
  (add-creature creature (or node
			     (random-elt (adjacent-nodes-of (node creature)))))
  (suspend creature +movement-time+))


(defmethod print-object ((cr creature) stream)
  (format stream "#<(Creature :Energy ~a)>" (energy cr)))



(defmethod suspend ((creature creature) &optional (time 1))
  "Escape from the interpeter in such a way that it can be resumed later."
  (schedule #'(lambda () (resume-creature creature)) (world creature) time)
  (signal 'suspend))

(define-condition interpreter-signal ()
  (creature)) 
(define-condition suspend (interpreter-signal)
  (reason duration)) 
(define-condition dead (interpreter-signal)
  (cause)
  (:documentation "He's dead jim"))

(define-condition stop (interpreter-signal)
  (return-val))

(define-condition creature-error (interpreter-signal error)
  ()) 

(defmethod interpret-creature ((creature creature))
  "Clear out any current state the creature had and start them anew."
  ;;TODO: Get fresh copies of control structure into place on creature
  ;;any other initial setup/resetting.
  (resume-creature creature))

(defmethod resume-creature ((creature creature))
  "Resume executing a creature that was previously suspended."
  (handler-case
      (progn
	;;TODO: interpret
	(interpret (current-machine-state creature))
	)
    (suspend (condition))
    (stop (condition))
    (death (condition))
    (error (error))))

