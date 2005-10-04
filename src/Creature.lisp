(in-package :rEvolver)

(defparameter +possible-creatures+ '())
(defparameter +movement-energy-ratio+ 1/10)


(defclass creature ()
  ((energy-level :accessor energy :initform 0)
   (position :reader position :initarg :position)
   (world :reader world :initarg :world)
   (turn-action)))

(defmethod use-energy ((creature creature) energy)
  (decf )) 

(defmethod add-creature ((creature creature) (location location))
  "add a creature to a location."
  (pushf creature (creatures location)))

(defmethod remove-creature ((creature creature) (location location))
  "Take a creature out of a location."
  (remove creature location :test #'eq))


;;;; Define-creature-method is a macro to create a method
;;;; specialized on a type of creature
(defmacro define-creature-op (name lambda-list &key documentation energy action ticks)
  "Ease in the creation of operations a creature can perform.
	For now I'm thinking this will mean having it specialized on a creature."
    ;;get the name of the symbol in the creatures package
  (let ((fn-name (intern (string name) :rEvolver.creature))
	(creature (gensym "creature")))
    `(defun ,name (,creature ,@lambda-list)
      ,documentation
      (schedule #'(lambda ()
		    )
       (world ,creature)
       ,ticks)
    ))
  )
  
  
(define-creature-op look ((direction nil))
  "Examine the location the creature is currently standing at if the location
	is null, or look in the specified direction.")

(define-creature-op move (direction)
  :documentation "Move the creature in a specified direction."
  :ticks 10
  :action (let ((l (position creature))
		(dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
	    (remove-creature creature l) 
	    (add-creature creature (funcall dirfn l))
	    creature)
  :energy ((* (energy-level creature) +movement-energy-ratio+)))

(define-creature-op feed ()
  "Feed from the energy source at the current location.")

(define-creature-op reproduce ()
  "Creature reproduction.")
