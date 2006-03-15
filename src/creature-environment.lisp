(in-package :rEvolver)

(declaim (optimize (debug 3)))

(defparameter +movement-energy-ratio+ 1/10)
(defparameter +movement-time+ 10)
(defparameter +feed-cost+ 2)
(defparameter +feed-time+ 2)


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
      (error (e) (error 'has-the-pox :original-error e)))))

(defparameter +base-lisp-environment+
  (let (env)
    (flet ((addenv (name fun)
	     (setf env (env-push name fun env))))
      (addenv 'dna:cons (cr-env-function (a b) (cons a b)))
      (addenv 'dna:car (cr-env-function (c) (car c)))
      (addenv 'dna:cdr (cr-env-function (c) (cdr c)))
      (addenv 'dna:or (cr-env-function (x y) (or x y)))
      (addenv 'dna:not (cr-env-function (n) (not n)))
      (addenv 'dna:eq (cr-env-function (x y) (eq x y)))
      (addenv 'dna:equal (cr-env-function (x y) (equal x y)))
      (addenv 'dna:if (cr-env-function (test x y) (if test x y)))
      (addenv 'dna:nil nil)
      (addenv 'dna:nil T)
      (mapcar (lambda (sym)
		(addenv sym sym))
	      '(dna:node dna:function dna:list dna:atom dna:number)))
    env))


(defmethod creature-environment ((creature creature))
  (let ((env +base-lisp-environment+))
    (flet ((addenv (name fun)
	     (setf env (env-push name fun env))))
      (addenv 'dna:move (cr-env-function (node)
			  (rlogger.dribble "Starting a move.")
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
			     (suspend creature k +movement-time+))
			   'dna:move)))
      (addenv 'dna:feed (cr-env-function ()
			  (rlogger.dribble "Starting to feed.")
			  (interrupt-interpreter/cc
			   (lambda (k)
			     (with-slots (node world energy) creature
			       (setf energy (+ energy (- (take-all-energy node) +feed-cost+)))
			       (suspend creature k +feed-time+)))
			   'dna:feed)))
      (addenv 'dna:energy? (cr-env-function ()
			     (let ((energy (> (energy (node creature)) 0)))
			       (rlogger.dribble "Querying node for energy? ~a." energy)
			       energy))))
    env))

