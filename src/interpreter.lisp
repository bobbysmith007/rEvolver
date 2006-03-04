(in-package :CSE)

(declaim (optimize (debug 3)))


(defun lookup (key environment)
  "Find a value in the environment by key."
  (cdr (assoc key environment)))
(defun env-push (key datum environment)
  (acons key datum environment))

(defun top (stack)
  (first stack))

(defun pop-control-op (control)
  (pop (car control)))

(defstruct closure
  environment
  name
  control)

(defstruct (frame (:constructor make-frame
				(control environment &optional previous-frame)))
  control
  environment
  previous-frame)

(defun make-interpreter (tree primary-environment &optional beta-reduction-cost)
  "Takes a standardized tree and returns a function that when invoked will
 commence interpret the tree in the primary environment. Reinnvokng the
 returned function will restart the interpretation."
  (let ((control (build-contrl-structures tree)))
    (lambda ()
      (start-CSE-machine control
			'()
			primary-environment
			beta-reduction-cost))))

(define-condition interrupt ()
  (continuation))
(define-condition unbound-name (error)
  (name environment))

(defun start-CSE-machine (control stack environment beta-reduction-cost)
  (labels ((build-rator-continuation ()
	     ;;the continuation function is just push 
	     (lambda (val)
	       (start-CSE-machine control
			  (cons val stack)
			  environment
			  beta-reduction-cost)))
	   (handle-interrupt (interrupt)
	     (funcall (slot-value interrupt 'continuation)
		      (build-rator-continuation))
	     (return-from start-CSE-machine)))

    (handler-bind ((interrupt #'handle-interrupt))
      
      (loop while control ;;when there is no l
	do ;(break "Top of loop")
	(if (null (top control))
	       ;;cse rule 5
	       (progn (pop control)
		      (pop environment))
	       
	       (let ((op (pop-control-op control)))
		 (cond
		   
		   ;;cse rule 2
		   ((closure-p op)
		    (setf (closure-environment op) environment)
		    (push op stack))
		   
		   ((eq op 'gamma)
		    (let* ((rator (pop stack))
			   (rand (pop stack)))
		      (if (closure-p rator)
			;;cse rule 4
			(progn
			  (push (closure-control rator) control)
			  (push (cons (closure-name rator) rand) closure-environment))
			;;cse rule 3
			;;if rator here gets #'interrupt-interpreter/cc here, it's allright
			(if (functionp rator)
			    (push (funcall rator rand) stack)
			    (error "We didn't have a function ~a ~a" rator rand)))))

		   ;; CSE Rule 1
		   ((numberp op)
		    (push op stack))
		   ((symbolp op)
		    (let ((obj (lookup op environment)))
		       (push obj stack)
		       ))
		   
		   (T (error "Unkwown operation on the control ~a" op)))))
	finally (return (top stack))
	))))



(defun interrupt-interpreter/cc (cont)
  "When called from a primary-environment function, will interrupt the currently
 running interpreter. The argument cont(inuation) should be a function of one
 argument. The function is called, passing in the interpreter continuation.
That continuation may then be saved off somewhere. Will return the result back
up to whoever originally invoked the interpreter.")




(defparameter +ST+ '(gamma
		     (lambda x (gamma (gamma + 1)
				      x))
		     3))
(defparameter +CTRL+ `( (3
			 ,(make-closure :name 'x
					:control '(x 1 + gamma gamma))
			 gamma)))
(defparameter +PE+ (env-push '+ #'(lambda (x)
				    (lambda (y) (+ x y)))
			     (env-push
			      '- #'(lambda (x)
				     (lambda (y) (- x y)))
			      nil)))



`(6 5 ,(make-closure :name 'x
		     :control (list (make-closure :name 'w
					    :control '(w x + gamma gamma))))
  gamma gamma
  )