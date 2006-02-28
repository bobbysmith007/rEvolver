(in-package :rEvolver)


(defmethod suspend ((creature creature) &optional (time 1))
  "Escape from the interpeter in such a way that it can be resumed later."
  (schedule #'(lambda () (resume-creature creature)) (world creature) time)
  (signal 'suspend))

(define-condition interpreter-signal ()
  (creature)) 
(define-condition suspend (interpreter-signal)
  (reason duration)) 
(define-condition death (interpreter-signal)
  (cause))
(define-condition stop (interpreter-signal)
  (return-val))

(define-condition creature-error (interpreter-signal error)
  ()) 

(defmethod interpret-creature ((creature creature))
  "Clear out any current state the creature had and start them anew."
  ;;TODO: Get fresh copies of control structure into place on creature
  ;;any other initial setup/resetting.
  (resume-creature creature))

(defmethod resume-creature ((creature creature))
  "Resume executing a creature that was previously suspended."
  (handler-case
      (progn
	;;TODO: interpret
	)
    (suspend (condition))
    (stop (condition))
    (death (condition))
    (error (error))))


