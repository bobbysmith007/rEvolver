(in-package :rEvolver)

(defparameter +initial-creature-count+ 50
  "The number of random creatures to initially populate the world with.")

(defclass ticker ()
  ((tick-number
    :initarg :tick-number
    :initform 0
    :accessor tick-number)))

(defgeneric advance-time (ticker)
  (:documentation "Move time forward for a ticked object."))

(defmethod advance-time (ticker)
  (incf (tick-number ticker)))

(defgeneric add (creature world)
  (:documentation "Add a creature to a world."))

