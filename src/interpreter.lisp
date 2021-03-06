(in-package :CSE)
;(declaim (optimize (debug 3)))
(defun lookup (key environment)
  "Find a value in the environment by key."
  (let ((val (assoc key environment)))
    (values (cdr val)
	    (not (null val)))))

(defun append-to-environment (key datum environment)
  (acons key datum environment))


(defstruct (unclosed-lambda (:constructor make-unclosed-lambda (name control)))
  "A closure needs to keep track of the variable name, the function body (control)
 and what environment it is closed. Since right now this object is created as an
 unclosed function that is later fixed to an environment the contstructor only
 takes the name and control."
  name
  control)

(defstruct (closure (:constructor make-closure (unclosed-lambda environment)))
  "A lambda in an environment"
  unclosed-lambda
  environment)


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
  (let ((unclosed-lambda (closure-unclosed-lambda closure)))
    (make-frame (unclosed-lambda-control unclosed-lambda)
		(append-to-environment (unclosed-lambda-name unclosed-lambda)
				       value
				       (closure-environment closure))
		previous)))

(defun peek-op (frame)
  (first (frame-control frame)))

(defun pop-op (frame)
  (let ((control (frame-control frame)))
    (setf (frame-control frame) (rest control))
    (first control)))

;;;;Code for controlling the execution of the interpreter.

(define-condition escape ()
  ((reason :initarg :reason :initform nil :accessor reason)))

(define-condition interrupt (escape)
  ((continuation :initarg :continuation)))

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
  ((name :initarg :name :reader unbound-name-name)
   (environment :initarg :environment :reader unbound-name-environment))
  (:report (lambda (condition stream)
	     (format stream "~a is unbound in ~a"
		     (unbound-name-name condition)
		     (unbound-name-environment condition)
		     ))))

(defun start-CSE-machine (frame stack beta-reduction-cost)
  (handler-case  
      (labels ((new-frame (closure val)
		 (setf frame (make-frame-from-closure closure val frame)))
	       (pop-frame ()
		 "Return to a previous frame, if there isn't one then we are finished
		and return the top of the stack upward."
		 (prog1 frame
		   (setf frame (frame-previous frame))
		   (unless frame
		     (return-from start-CSE-machine
		       (values (first stack)
			       (rest stack))))))
	   
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
		 (let ((val (funcall (slot-value interrupt 'continuation)
				     (build-rator-continuation))))
		   (return-from start-CSE-machine val))))

	(handler-bind ((interrupt #'handle-interrupt))
	  (loop 
	      do 
	      (if (null (peek-op frame))
		  ;;cse rule 5, exit an environment.
		  (pop-frame);;this is our loop exit
	    
		  (let ((op (pop-op frame)))
		    (cond
		
		      ;;cse rule 2 stack a lambda
		      ((unclosed-lambda-p op)
		       (push-stack (make-closure op (frame-environment frame))))
		
		      ((eq op 'dna:gamma)
		       (let* ((rator (pop-stack))
			      (rand (pop-stack)))
			 ;;we are performing a beta-reduction. let the outside environment know.
			 ;; this could possibly escape... ok
			 (when beta-reduction-cost (funcall beta-reduction-cost))
			 (cond ((closure-p rator)
				;;cse rule 4 apply lambda
				(new-frame rator rand))
			       ;;cse rule 3 apply rator
			       ;;if rator here gets #'interrupt-interpreter/cc here, it's allright
			       ;;the push will be handled by the build-rator-continuation stuff.
			       ((functionp rator)
				(push-stack (funcall rator rand)))
			       (T ;(break "~a ~a" rator rand)
				  (error 'invalid-gamma-application
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
		   
		      (T (error "Unknown operation on the control ~a" op))))))))
    ;(escape (esc)
;	    ;;we resignal escape, hoping some outer handler is there,
;	    ;;if it isn't handled we return.
;	    (signal esc)
;	    (return-from start-CSE-machine (reason esc)))
    ))



(defun make-interpreter (flattened-tree primary-environment
					&optional beta-reduction-cost)
  "Takes a standardized tree and returns a function that when invoked will
 commence interpret the tree in the primary environment. Reinnvokng the
 returned function will restart the interpretation.
The beta-reduction is a function that can perform other side effects when any beta-reduction
 is performed by the interpreter."
  
  (labels ((root ()
	     "This is root interpreter continuation."
	     (let ((frame (make-frame
			   flattened-tree
			   primary-environment
			   nil
			  ))
		   (stack '()))
	       (start-CSE-machine frame stack beta-reduction-cost)))
	   ;(root (fn)
;	     "This is root interpreter continuation."
;	     (let ((frame (make-frame
;			   flattened-tree
;			   primary-environment
;			   (make-frame '(dna:gamma) nil nil)))
;		   (stack (list fn)))
;	       (start-CSE-machine frame stack beta-reduction-cost)))
	   )
    #'root))


(defun lchild (tree)
  (second tree))
(defun rchild (tree)
  (third tree))
(defun root (tree)
  (first tree))

(defun flattener (tree)
  (labels ((rflat (tree )
	     (cond
	       ((atom tree) (list tree))

	       ((eq (root tree) 'dna:lambda)
		(list (make-unclosed-lambda (lchild tree)
					    (rflat (rchild tree)))))
	       ((eq (root tree) 'dna:gamma)
		(nconc (rflat (rchild tree)) ;operator
		       (rflat (lchild tree)) ;operand
		       (list 'dna:gamma)))
	       (T (error "Found node ~a internally on a standardized tree."
			 (root tree))))))
    (rflat tree )))

