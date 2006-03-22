(in-package :CSE)

;(declaim (optimize (debug 3)))


(defun lookup (key environment)
  "Find a value in the environment by key."
  (let ((val (assoc key environment)))
    (values (cdr val)
	    (not (null val)))))

(defun env-push (key datum environment)
  (acons key datum environment))

(defstruct (closure (:constructor make-lambda (name control)))
  "A closure needs to keep track of the variable name, the function body (control)
 and what environment it is closed. Since right now this object is created as an
 unclosed function that is later fixed to an environment the contstructor only
 takes the name and control."
  environment
  name
  control)


(defstruct (frame (:constructor make-frame
				(control environment previous)))
  "a frame is an evaluation of some code in an environment.
	aka stack-frame, function call"
  control
  environment
  previous)

(defun make-frame-from-closure (closure value &optional previous)
  "Most of the new frames are going to be created when we evaluate a closed fun.
This function creates a new frame that will evaluate the body of the closure,
with the environment it was closed to plus the name+argument pair."
  (make-frame (closure-control closure)
	       (env-push (closure-name closure) value (closure-environment closure))
	       previous))

(defun peek-op (frame)
  (first (frame-control frame)))

(defun pop-op (frame)
  (let ((control (frame-control frame)))
    (setf (frame-control frame) (rest control))
    (first control)))

(defun escape (error)
  (revolver::rlogger.dribble "Escaping: ~a" error)
  (let ((esc (find-restart 'escape)))
    (when esc
      (invoke-restart esc error))))

(define-condition interrupt ()
  ((continuation :initarg :continuation)
   (reason :initarg :reason :initform nil :accessor reason)))

(defun interrupt-interpreter/cc (cont &optional reason)
  "When called from a primary-environment function, will interrupt the currently
 running interpreter. The argument cont(inuation) should be a function of one
 argument. The function is called, passing in the interpreter continuation.
That continuation may then be saved off somewhere. Will return the result back
up to whoever originally invoked the interpreter."
  (signal 'interrupt :continuation cont :reason reason))

(define-condition code-error (error)
  ((original-error :initarg :original-error :accessor original-error)))
(define-condition invalid-gamma-application (code-error)
  ((rator :initarg :rator)
   (rand :initarg :rand)))
(define-condition unbound-name (code-error)
  ((name :initarg :name)
   (environment :initarg :environment)))

(defun start-CSE-machine (frame stack beta-reduction-cost)
  (restart-case 
   (labels ((new-frame (closure val)
	      (setf frame (make-frame-from-closure closure val frame)))
	    (pop-frame ()
	      "Return to a previous frame, if there isn't one then we are finished
		and return the top of the stack upward."
	     (prog1 frame
	       (setf frame (frame-previous frame))
	       (unless frame
		 (return-from start-CSE-machine
		   (progn ;(break "Done, about to return." stack)
			  (values (first stack)
				  (rest stack)))))))
	   
	   (push-stack (val)
	     (setf stack (cons val stack)))
	   (pop-stack ()
	     (prog1 (first stack)
	       (setf stack (rest stack))))
	   (frame-lookup (key)
	     (apply #'values
		    (multiple-value-list
			(lookup key (frame-environment frame)))))
	   
	   (build-rator-continuation ()
	     "Make a continuation that will push the return value onto the stack
		and continue processing."
	     (lambda (val)
	       (start-CSE-machine frame
				  (cons val stack)
				  beta-reduction-cost)))
   
	   (handle-interrupt (interrupt)
	     "Interrupt the current machine by calling the interrupt's continutation
		passing it the interpreter continuation and finally returning it's value to the top.
		the result up to the whomever originally invoked the machine."
	     (revolver::rlogger.dribble "Interrupting interpreter: ~a" (reason interrupt))
	     (return-from start-CSE-machine
	       (funcall (slot-value interrupt 'continuation)
			(build-rator-continuation)))))

    (handler-bind ((interrupt #'handle-interrupt))
      (loop 
	do 
	(if (null (peek-op frame))
	    ;;cse rule 5, exit an environment.
	    (pop-frame) ;;this is our loop exit
	    
	    (let ((op (pop-op frame)))
	      (cond
		
		;;cse rule 2 stack a lambda
		((closure-p op)
		 (setf (closure-environment op) (frame-environment frame))
		 (push-stack op))
		
		((eq op 'dna:gamma)
		 (let* ((rator (pop-stack))
			(rand (pop-stack)))
		   ;;we are performing a beta-reduction. let the outside environment know.
		   ;; this could possibly escape... ok
		   (funcall beta-reduction-cost)
		   (cond ((closure-p rator)
			  ;;cse rule 4 apply lambda
			  (new-frame rator rand))
			 ;;cse rule 3 apply rator
			 ;;if rator here gets #'interrupt-interpreter/cc here, it's allright
			 ;;the push will be handled by the build-rator-continuation stuff.
			 ((functionp rator)
			  (push-stack (funcall rator rand)))
			 (T (error 'invalid-gamma-application
				   :rator rator
				   :rand rand)))))

		   ((numberp op)
		    (push-stack op))
		   
		   ;; CSE Rule 1 Stack a name
		   ((symbolp op)
		    (multiple-value-bind (obj foundp) (frame-lookup op)
		      (if foundp
			  (push-stack obj)
			  (error 'unbound-name
				 :name op
				 :environment (frame-environment frame)))))
		   
		   (T (error "Unkwown operation on the control ~a" op))))))))
   (escape (error) error)))



(defun make-interpreter (standardized-tree primary-environment &optional beta-reduction-cost)
  "Takes a standardized tree and returns a function that when invoked will
 commence interpret the tree in the primary environment. Reinnvokng the
 returned function will restart the interpretation.
The beta-reduction is a function that can perform other side effects when any beta-reduction
 is performed by the interpreter."
  (let* ((flattened (flattener standardized-tree))
	 (frame (make-frame flattened
			    primary-environment nil)))
    (lambda ()
      (start-CSE-machine frame
			'()
			beta-reduction-cost))))


(defun lchild (tree)
  (second tree))
(defun rchild (tree)
  (third tree))
(defun root (tree)
  (first tree))

(defun flattener (tree)
  (labels ((rflat (tree )
	     (if (and tree (listp tree))
		 (let ((node (root tree)))
		   (cond
		     ((listp node)
		      (append (rflat (rchild tree))
			      (rflat (lchild tree))
			      (rflat node)))
		     ((eq node 'dna:lambda)
		      (list (make-lambda (lchild tree) (rflat (rchild tree)))))
		     ((eq node 'dna:gamma)
		      (append (rflat (rchild tree))
			      (rflat (lchild tree))
			      '(dna:gamma)))
		     (T (error "Found node ~a internally on a standardized tree." node))))
		 (list tree))))
    (append (rflat tree ) (list 'dna:eof))))
