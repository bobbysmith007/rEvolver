(in-package :rEvolver)

(defparameter +possible-creatures+ '())

(defclass creature () '())



;;;;Functions available for a creature

;;;; Define-creature-method is a macro to create a method
;;;; specialized on a type of creature
(defmacro define-creature-op (name lambda-list &body body)
  "Ease in the creation of operations a creature can perform.
	For now I'm thinking this will mean having it specialized on a creature.")


(define-creature-op look ((direction nil))
  "Examine the location the creature is currently standing at if the location
is nill, or look in the specified direction.")

(define-creature-op move (direction)
  "Move the creature in a specified direction.")

(define-creature-op feed ()
  "Feed from the energy source at the current location.")

(define-creature-op reproduce ()
  "Creature reproduction.")
