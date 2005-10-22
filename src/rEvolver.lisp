(in-package :rEvolver)



(defclass ticker ()
  ((tick-number
    :initarg :tick-number
    :initform 0
    :accessor tick-number)))

(defgeneric advance-time (ticker)
  (:documentation "Move time forward for a ticked object."))

(defmethod advance-time ((ticker ticker))
  (incf (tick-number ticker)))

(defgeneric add (creature world)
  (:documentation "Add a creature to a world."))

(defclass tick-list (ticker)
  ((actions-list :initform '()
		  :accessor actions
		  :documentation "The lambda functions to be run at any given tick.")))

(defmethod push-tl (action (tl tick-list))
  "Push a creature onto a tick-list and return the tick-list"
  (with-accessors ((actions actions)) tl
		  (push action actions))
  tl)

(defmethod schedule (action queue tick)
  (labels ((scan (q)
	   (cond
	     ((null q)
	      (list (push-tl action (make-instance 'tick-list :tick-number tick))))
	     ((= tick (tick-number (first q)))
	      (push-tl action (first q))
	      q)
	     ((> tick (tick-number (first q)))
	      (cons (first q) (scan (rest q))))
	     (T
	      (cons (push-tl action (make-instance 'tick-list :tick-number tick)) q)))))
    (scan queue)))

(define-condition escape ()
  ()
  (:documentation "A condition for escaping out of the "))

(defmethod execute ((tick-list tick-list))
  (format-log "At tick(~a) count = ~a~%" (tick-number tick-list) (length (actions tick-list)))
  (dolist (action (actions tick-list))
    (handler-case (funcall action)
      (dead (cr) (format-log "Dead: ~a~%" (creature cr)))
      (escape () (format-log "escaped!~%"))
      )))