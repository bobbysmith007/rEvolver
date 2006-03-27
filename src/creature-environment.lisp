(in-package :rEvolver)

;;(declaim (optimize (debug 3)))
(defvar *function-energy-cost-hash* (make-hash-table ))
(defvar *function-time-cost-hash* (make-hash-table ))
(defvar *default-energy-cost* nil)

(defmacro curry (args &body body)
  "Turn a function of arity n into max(arity, 1) functions.
That is, make sure there is at least arg, dummy if necessary, and curry functions
of higher arity."
  (labels ((rcurry (args)
	     (if (null args)
		 body
		 `((lambda (,(first args))
		     ,@(rcurry (rest args)))))))
    (first (rcurry (or args (list 'dummy))))))

(defmacro cr-env-function (args &body body)
  `(curry ,args
    (handler-case (progn ,@body)
      ;;TODO: good place for logging?
      (error (e) (error 'CSE:code-error :original-error e)))))

(defun make-creature-base-lisp-environment ()
  (let (env)
    (flet ((pushenv (name val)
	     (setf env (append-to-environment name val env))))
      (macrolet ((define-environment-fun (name args &body body)
		     `(pushenv ',name
		       (cr-env-function ,args ,@body))))
	(define-environment-fun dna:cons (a b) (cons a b))
	(define-environment-fun dna:car (c) (car c))
	(define-environment-fun dna:cdr (c) (cdr c))
	(define-environment-fun dna:or (x y) (or x y))
	(define-environment-fun dna:not (n) (not n))
	(define-environment-fun dna:eq (x y) (eq x y))
	(define-environment-fun dna:equal (x y) (equal x y))
	(define-environment-fun dna:if (test x y) (if test x y))
	(pushenv 'dna:nil nil)
	(pushenv 'dna:T T)
	(mapcar (lambda (sym)
		  (pushenv sym sym))
		'(dna:eof dna:node dna:function dna:list dna:atom dna:number)))
      env)))

(defun apply-energy-costs (name creature)
  "Based on operation name, have the creature use the appropriate energy."
  (multiple-value-bind (val in-hash) (gethash name *function-energy-cost-hash*)
    (when in-hash
      (use-energy creature val))))

(defun apply-time-costs (name creature continuation cont-arg)
  "Based on operation name, continue the creature at the appropriate-time."
  (multiple-value-bind (val in-hash) (gethash name *function-time-cost-hash*)
    (if  in-hash
	 (reschedule creature
		     (lambda ()
		       (funcall continuation cont-arg))
		     val)
	 (funcall continuation cont-arg)))
  name)


;;TODO: The continuations given to us by the interpreter are functions
;; of an argument, we need to treat them as such.
(defmethod creature-environment ((creature creature))
  (let ((env (make-creature-base-lisp-environment)))
    (macrolet ((costly-cr-env-function (name args cont-arg &body body)
		 (with-unique-names (k)
		   `(pushenv ',name
		     (cr-env-function ,args
		       (rlogger.dribble "Starting: ~a" ',name)
		       (interrupt-interpreter/cc
			(lambda (,k)
			  (apply-energy-costs ',name creature)
			  (apply-time-costs ',name creature ,k (progn ,@body)))))))))
      
      (flet ((pushenv (name fun)
	       (setf env (append-to-environment name fun env))))
 
	(costly-cr-env-function dna:move (node) 
				(let ((previous-node (node creature)))
				  (remove-creature creature previous-node)
				  ;;if the creature didn't specify then pick a random direction.
				  (add-creature creature
						(or node
						    (random-elt (adjacent-nodes-of
								 previous-node))))
				  (node creature)))
 
	(costly-cr-env-function dna:feed () 
				(with-slots (node world energy) creature
				  (setf energy (+ energy (take-all-energy node))))
				(energy creature))
 
	(costly-cr-env-function dna:energy? () 
				(let ((energy (> (revolver.map::energy (node creature)) 0)))
				  (rlogger.dribble "Querying node for energy resulted in: ~a." energy)
				  energy))
 
	(costly-cr-env-function dna:asexually-reproduce ()
				(asexually-reproduce creature)
				T)
	))
    env))



(defmethod asexually-reproduce ((golem creature))
  (rlogger.info "WOOHOO! Reproduction! ~a " golem)
  (with-slots (max-energy mutation-rate value-mutation-rate dna mutation-depth)
      golem
    (let ((cr (make-instance 'creature
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
      (schedule #'(lambda ()
		    (rlogger.dribble "We are about to animate a NEW CREATURE! ~a" cr)
		    (animate cr))
		(world golem)
		(or (gethash 'dna:asexually-reproduce *function-time-cost-hash*)
		    1)
		))))