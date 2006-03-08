(in-package :rEvolver)

(defparameter +movement-energy-ratio+ 1/10)
(defparameter +movement-time+ 10)
(defparameter +feed-cost+ 2)
(defparameter +feed-time+ 2)

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
  (push creature (creatures-of node))
  (setf (node creature) node))

(defmethod remove-creature ((creature creature) (node node))
  "Take a creature out of a node."
  (setf (creatures-of node) (delete creature (creatures-of node) :test #'eq))
  (setf (node creature) nil))


(defmacro curry (args &body body)
  (labels ((rcurry (args)
	     (if (null args)
		 body
		 `((lambda (,(first args))
		   ,@(rcurry (rest args)))))))
    (first (rcurry (or args (list 'dummy))))))

(defparameter +base-lisp-environment+
  (let (env)
    (flet ((addenv (name fun)
	     (setf env (env-push name fun env))))
      (addenv 'dna:cons (curry (a b) (cons a b)))
      (addenv 'dna:car (curry (c) (car c)))
      (addenv 'dna:cdr (curry (c) (cdr c)))
      (addenv 'dna:or (curry (x y) (or x y)))
      (addenv 'dna:not (curry (n) (not n)))
      (addenv 'dna:eq (curry (x y) (eq x y)))
      (addenv 'dna:equal (curry (x y) (equal x y)))
      (addenv 'dna:if (curry (test x y) (if test x y))))))


(defmethod creature-environment ((creature creature))
  (let ((env +base-lisp-environment+))
    (flet ((addenv (name fun)
	     (setf env (env-push name fun env))))
      (addenv 'dna:move (curry (node)
			  (interrupt-interpreter/cc
			   (lambda (k)
			     ;;before we move them to the new node use the energy (which might kill them)
			     (use-energy creature
					 (* (energy creature)
					    +movement-energy-ratio+))  
			     (remove-creature creature (node creature))
			     ;;if the creature didn't specify then pick a random direction.
			     (add-creature creature
					   (or node
					       (random-elt (adjacent-nodes-of
							    (node creature)))))
			     (schedule k (world creature) +movement-time+)))))
      (addenv 'dna:feed (curry ()
			  (interrupt-interpreter/cc
			   (lambda (k)
			     (with-slots (node world energy) creature
			       (setf energy (+ energy (- (take-all-energy node) +feed-cost+)))
			       (schedule k world +feed-time+))))))
      (addenv 'dna:energy? (()
			    (> (energy (node creature)) 0))))
    env))


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

