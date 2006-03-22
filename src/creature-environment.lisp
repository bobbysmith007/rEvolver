(in-package :rEvolver)

;;(declaim (optimize (debug 3)))
(defvar *function-energy-cost-hash*)
(defvar *function-time-cost-hash*)
(defvar *default-energy-cost*)

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
    (flet ((addenv (name fun)
	     (setf env (env-push name fun env))))
      (macrolet ((make-env-fn (name args &body body)
		   `(addenv ',name
			   (cr-env-function ,args

			     ,@body))		     
		   ))
	(make-env-fn dna:cons (a b) (cons a b))
	(make-env-fn dna:car (c) (car c))
	(make-env-fn dna:cdr (c) (cdr c))
	(make-env-fn dna:or (x y) (or x y))
	(make-env-fn dna:not (n) (not n))
	(make-env-fn dna:eq (x y) (eq x y))
	(make-env-fn dna:equal (x y) (equal x y))
	(make-env-fn dna:if (test x y) (if test x y))
	(addenv 'dna:nil nil)
	(addenv 'dna:T T)
	(mapcar (lambda (sym)
		  (addenv sym sym))
		'(dna:eof dna:node dna:function dna:list dna:atom dna:number)))
      env)))

(defun apply-energy-costs (name creature)
  (multiple-value-bind (val in-hash) (gethash name *function-energy-cost-hash*)
    (when in-hash
      (use-energy creature val))))

(defun apply-time-costs (name creature continuation cont-arg)
  (multiple-value-bind (val in-hash) (gethash name *function-time-cost-hash*)
    (if  in-hash
      (suspend creature
	       (lambda ()
		 (funcall continuation cont-arg))
	       val)
      (funcall continuation cont-arg))))


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
		    (animate cr))
		(world golem)
		(or (gethash 'dna:asexually-reproduce *function-time-cost-hash*)
		    1)
		    ))))

;;TODO: The continuations given to us by the interpreter are functions
;; of an argument, we need to treat them as such.
(defmethod creature-environment ((creature creature))
  (let ((env (make-creature-base-lisp-environment)))
    (macrolet ((costly-cr-env-function (name args cont-arg &body body)
		 (with-unique-names (k)
		   `(addenv ',name
		     (cr-env-function ,args
		     (rlogger.dribble "Starting: ~a" ',name)
		     (interrupt-interpreter/cc
		      (lambda (,k)
			(apply-energy-costs ',name creature)
			,@body
			(apply-time-costs ',name creature ,k ,cont-arg))))))))
      
      (flet ((addenv (name fun)
	       (setf env (env-push name fun env))))
 
	(costly-cr-env-function dna:move (node) (node creature)
	  (let ((previous-node (node creature)))
	    (remove-creature creature previous-node)
	    ;;if the creature didn't specify then pick a random direction.
	    (add-creature creature
			  (or node
			      (random-elt (adjacent-nodes-of
					   previous-node))))))
 
	(costly-cr-env-function dna:feed () (energy creature)
	  (with-slots (node world energy) creature
	    (setf energy (+ energy (take-all-energy node)))))
 
	(costly-cr-env-function dna:energy? () T
	  (let ((energy (> (revolver.map::energy (node creature)) 0)))
	    (rlogger.dribble "Querying node for energy resulted in: ~a." energy)
	    energy))
 
	(costly-cr-env-function dna:asexually-reproduce () T
          (asexually-reproduce creature))
      ))
    env))

