(in-package :rEvolver)

(defparameter +movement-energy-ratio+ 1/10)
(defparameter +movement-time+ 10)

(defclass creature ()
  ((energy :accessor energy :initform 0 :initarg :energy)
   (node :accessor node :initarg :node)
   (world :reader world :initarg :world)
   (decision-fn :accessor decision-fn)
   ;;we're eventually going to be storing CSE runtime info here.
   stack
   input
   ))

(defmethod die ((creature creature))
  (when (node creature)
    (rlogger.info "[~a] Creature died: ~a"
		  (tick-number (world creature))
		  creature)
    (remove-creature creature (node creature)))
  creature)

(define-condition dead ()
  ((creature :initarg :creature :accessor creature)))

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