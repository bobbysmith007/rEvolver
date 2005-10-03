(in-package :rEvolver)

(defparameter +possible-creatures+ '())

(defclass creature ()
  ((energy-level :reader energy :initform 0)
   (position :reader position :initarg :position)))

(defmethod add-creature ((creature creature) (location location))
  "add a creature to a location."
  (pushf creature (creatures location)))

(defmethod remove-creature ((creature creature) (location location))
  "Take a creature out of a location."
  (remove creature location :test #'eq))

;;;;Functions available for a creature

(defgeneric energy-level (creature )
  (:documentation "How much energy does the creature currently have."))

;;;; Define-creature-method is a macro to create a method
;;;; specialized on a type of creature
(defmacro define-creature-op (name lambda-list &body body)
  "Ease in the creation of operations a creature can perform.
	For now I'm thinking this will mean having it specialized on a creature."
  (push '(creature creature) lambda-list)
  (let ((doc (when (stringp (first body)) (pop body))))
    `(defmethod ,name ,lambda-list ,doc ,@body)))

(define-creature-op look ((direction nil))
  "Examine the location the creature is currently standing at if the location
	is null, or look in the specified direction.")

(define-creature-op move (direction)
  "Move the creature in a specified direction."
  (let ((l (position creature))
	(dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
    (remove-creature creature l) 
    (add-creature creature (funcall dirfn l))
    creature))

(define-creature-op feed ()
  "Feed from the energy source at the current location.")

(define-creature-op reproduce ()
  "Creature reproduction.")
