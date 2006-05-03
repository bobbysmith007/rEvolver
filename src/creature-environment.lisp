(in-package :rEvolver)

;;(declaim (optimize (debug 3)))


(defmacro curry (args &body body)
  "Turn a function of arity n into max(arity, 1) functions.
That is, make sure there is at least arg, dummy if necessary, and curry functions
of higher arity."
  (with-unique-names (dummy)
    (labels ((rcurry (args)
	       (if (null args)
		   body
		   `((lambda (,(first args))
		       (declare (ignorable ,(first args)))
		       ,@(rcurry (rest args)))))))
      (first (rcurry (or args (list dummy)))))))

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
	(define-environment-fun dna:< (a b) (< a b))
	(define-environment-fun dna:+ (a b) (+ a b))
	(define-environment-fun dna:- (a) (- a)) 
	(pushenv 'dna:nil nil)
	(pushenv 'dna:T T)
	(mapcar (lambda (sym)
		  (pushenv sym sym))
		'(dna:eof dna:node dna:function dna:list dna:atom dna:number)))
      env)))

(defun apply-time-costs (name creature continuation cont-arg)
  "Based on operation name, continue the creature at the appropriate-time."
  (let ((val (function-time-cost name *simulation*)) )
    (if  (and val (> val 0))
	 (reschedule creature
		     (lambda ()
		       (rlogger.dribble "About to resume from ~a with ~a as the value."
					name
					cont-arg)
		       (funcall continuation cont-arg))
		     val name)
	 
	 (funcall continuation cont-arg))
    (values name val))
  
  )


(defmethod creature-environment ((creature creature))
  (let ((env (make-creature-base-lisp-environment)))
    (flet ((pushenv (name fun)
	     (setf env (append-to-environment name fun env))))
      (macrolet ((costly-cr-env-function (name args &body body)
		   (with-unique-names (k)
		     `(pushenv ',name
		       (curry ,args
			 (rlogger.dribble "Starting: ~a on ~a " ',name creature)
			 (interrupt-interpreter/cc
			  (lambda (,k)
			    (use-energy creature
					(function-energy-cost ',name *simulation*))
			    
			    (apply-time-costs
			     ',name creature
			     ,k
			     (handler-case (progn ,@body)
			       (error (e) (error 'CSE:code-error :original-error e))))))
			  (error "I dont think we should ever get here if we are properly interrupting")
			 )))))

	(pushenv 'dna:me creature)
	(costly-cr-env-function dna:move (node) 
				(let* ((previous-node (node creature))
				      ;;if the creature didn't specify then pick a random direction.
				      (new-loc (or (and node
							(subtypep (type-of node) 'node )
							node) ;; so that the  correct value is returned
						   (random-elt
						    (adjacent-nodes-of previous-node)))))
				  (remove-creature creature previous-node)

				  (add-creature creature new-loc)
				  (node creature)))
 
	(costly-cr-env-function dna:feed () 
				(with-slots (node world max-energy energy) creature
				  (let* ((energy-gleened
					  (take-energy node
						       (- (max-energy creature)
							  energy)))
					 (new-creature-energy
					  (min (+ energy energy-gleened)
					       (max-energy creature))))
				    (setf energy new-creature-energy )))
				(energy creature))
	(costly-cr-env-function dna:energy? ()
				"Get the current energy of the node the creature is on."
				(energy (node creature)))
	(costly-cr-env-function dna:energy (thing)
				(etypecase thing
v v v v v v v
				  ((or creature node) (energy thing))
				  ))
	
	(costly-cr-env-function dna:look-at ()
				"get the list of adjacent nodes"
				(adjacent-nodes-of (node creature)))
*************
v v v v v v v
				  ((or creature node) (energy thing))
				  ))
*************
				  (node (energy thing))
				  (creature (energy creature))))
	(costly-cr-env-function dna:look-at ()
				"get the list of adjacent nodes"
				(adjacent-nodes-of (node creature)))
^ ^ ^ ^ ^ ^ ^
^ ^ ^ ^ ^ ^ ^
 
	(costly-cr-env-function dna:asexually-reproduce ()
				(asexually-reproduce creature)
				T)
	))
    env))


(defmethod asexually-reproduce ((golem creature))
  (rlogger.debug "WOOHOO! Reproduction! ~a " golem)
  ;;reducing the energy of the creature is taken care of by the costly-creature-fn.
  (let ((cr (clone-with-mutation golem)))
    (reschedule cr (creature-fn cr)
		(function-time-cost 'dna:asexually-reproduce *simulation*))))