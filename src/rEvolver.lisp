(in-package :rEvolver)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *debug-level* +info+ ))

;;;;Define the loggers we will be using
(deflogger rlogger ()
  :level *debug-level*
  :appender (make-instance 'verbose-stream-log-appender :stream t))

;;;; Ticker is everything in the system that keeps track of time.
(defclass ticker ()
  ((tick-number
    :initarg :tick-number
    :initform 0
    :accessor tick-number)))

(defgeneric advance-time (ticker)
  (:documentation "Move time forward for a ticked object."))

(defmethod advance-time ((ticker ticker))
  (incf (tick-number ticker)))


(defparameter revolver:Creature-dna nil)