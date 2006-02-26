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

(defmethod schedule (action queue tick)
  (meld queue (make-instance 'leftist-tree-node :key tick  :data action)))

(define-condition escape ()
  ()
  (:documentation "A condition for escaping out of the "))

(defmethod execute-1 (tick action)
  (format *log* "At tick(~a) action = ~a~%" tick action)
  (handler-case (funcall action)
    (dead (cr) (format *log* "Dead: ~a~%" (creature cr)))
    (escape () (format *log* "escaped!~%"))
    ))
