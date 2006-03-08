(in-package :CSE)

(declaim (optimize (debug 3)))


(defun lookup (key environment)
  "Find a value in the environment by key."
  (cdr (assoc key environment)))
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


(define-condition interrupt ()
  ((continuation :initarg :continuation)))
(define-condition unbound-name (error)
  (name environment))

(defun start-CSE-machine (frame stack beta-reduction-cost)
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
	     (lookup key (frame-environment frame)))
	   
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
	     (return-from start-CSE-machine
	       (funcall (slot-value interrupt 'continuation)
			(build-rator-continuation)))))

    (handler-bind ((interrupt #'handle-interrupt))
      (loop 
	do 
	(if (null (peek-op frame))
	    ;;cse rule 5, exit an environment.
	    (pop-frame)
	    
	    (let ((op (pop-op frame)))
	      (cond
		
		;;cse rule 2 stack a lambda
		((closure-p op)
		 (setf (closure-environment op) (frame-environment frame))
		 (push-stack op))
		
		((eq op 'gamma)
		 (let* ((rator (pop-stack))
			(rand (pop-stack)))
		   (cond ((closure-p rator)
			  ;;cse rule 4 apply lambda
			  (new-frame rator rand))
			 ;;cse rule 3 apply rator
			 ;;if rator here gets #'interrupt-interpreter/cc here, it's allright
			 ;;the push will be handled by the build-rator-continuation stuff.
			 ((functionp rator)
			  (push-stack (funcall rator rand)))
			 (T (error "We didn't have a function ~a ~a" rator rand)))))

		   ((numberp op)
		    (push-stack op))
		   
		   ;; CSE Rule 1 Stack a name
		   ((symbolp op)
		    (let ((obj (frame-lookup op)))
		      (unless obj
			(break "Environment lookup failed. Key: ~a  Frame: ~a"
			       op
			       frame))
		      (push-stack obj)))
		   
		   (T (error "Unkwown operation on the control ~a" op)))))))))



(defun interrupt-interpreter/cc (cont)
  "When called from a primary-environment function, will interrupt the currently
 running interpreter. The argument cont(inuation) should be a function of one
 argument. The function is called, passing in the interpreter continuation.
That continuation may then be saved off somewhere. Will return the result back
up to whoever originally invoked the interpreter."
  
  (signal 'interrupt :continuation cont ))


(defun make-interpreter (tree primary-environment &optional beta-reduction-cost)
  "Takes a standardized tree and returns a function that when invoked will
 commence interpret the tree in the primary environment. Reinnvokng the
 returned function will restart the interpretation."
  (let ((frame (make-frame tree primary-environment nil)))
    (lambda ()
      (start-CSE-machine frame
			'()
			beta-reduction-cost))))

(defparameter +ST+ '(gamma
		     (lambda x (gamma (gamma + 1)
				      x))
		     3))

(defparameter +CTRL1+ `(3 ,(make-lambda 'x '(x 1 + gamma gamma)) gamma))

(defparameter +CTRL2+ `(6 5 ,(make-lambda 'x
				(list (make-lambda 'w
						   '(w x + gamma gamma))))
			gamma gamma))
(defparameter +CTRL3+ `(7
			,(make-lambda 'z
				      '(z 2 * gamma gamma))
			gamma
			,(make-lambda 'x
				      `(x
					,(make-lambda 'w
						      '(w neg gamma))
					gamma 1 + gamma gamma))
			gamma))

(defparameter +CTRLk+ `(6 5 ,(make-lambda 'x
				`(,(make-lambda 'w
						'(w x + gamma gamma))
				  print-and-pause
				  gamma))
			gamma gamma))

(defparameter +PE+ `((+ . ,#'(lambda (x)
			       (lambda (y) (+ x y))))
		     (- . ,#'(lambda (x)
			       (lambda (y) (- x y))))
		     (neg . ,#'(lambda (x)
				 (- x)))
		     (* . ,#'(lambda (x)
			       (lambda (y) (* x y))))
		     (print-and-pause . ,#'(lambda (x)
					     (interrupt-interpreter/cc
					      (lambda (k)
						(print x)
						(lambda () (funcall k x))))))))


