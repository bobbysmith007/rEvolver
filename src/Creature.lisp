(in-package :rEvolver)

(defparameter +possible-creatures+ '())
(defparameter +movement-energy-ratio+ 1/10)


(defclass creature ()
  ((energy-level :accessor energy :initform 0)
   (location :reader location :initarg :location)
   (world :reader world :initarg :world)
   (turn-action)))

(defmethod use-energy ((creature creature) energy)
  (when (>= 0 (decf (energy-level creature) energy))
    (signal 'dead))) 

(defmethod add-creature ((creature creature) (location location))
  "add a creature to a location."
  (pushf creature (creatures location)))

(defmethod remove-creature ((creature creature) (location location))
  "Take a creature out of a location."
  (remove creature location :test #'eq))


(defmacro define-creature-op (name lambda-list &key documentation energy action ticks)
  "Ease in the creation of operations a creature can perform.
	For now I'm thinking this will mean having it specialized on a creature."
    ;;get the name of the symbol in the creatures package
  (let ((fn-name (intern (string name) :rEvolver.creature))
	(creature (gensym "creature")))
    `(defun ,name (,creature ,@lambda-list)
      ,documentation
      (schedule #'(lambda ()
		    ,@action
		    (use-energy ,creature ,energy )
		    )
       (world ,creature)
       ,ticks)
    ))
  )


;;;; Define-creature-method is a macro to create a method
;;;; specialized on a type of creature
 
  
(define-creature-op look ((direction nil))
  "Examine the location the creature is currently standing at if the location
	is null, or look in the specified direction.")

(define-creature-op move (direction)
  :documentation "Move the creature in a specified direction."
  :ticks 10
  :action (let ((l (location creature))
		(dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
	    (remove-creature creature l) 
	    (add-creature creature (funcall dirfn l))
	    creature)
  :energy (* (energy-level creature) +movement-energy-ratio+))

(defmethod move ((creature creature) direction)
  "Move the creature."
  (schedule #'(lambda ()
		(let ((l (location creature))
		      (dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
		  (remove-creature creature l)
		  (add-creature creature (funcall dirfn l)))
		(use-energy creature (* (energy-level creature) +movement-energy-ratio+)))
	    (world creature)
	    10)
  (signal 'escape))

(define-creature-op feed ()
  "Feed from the energy source at the current location.")

(define-creature-op reproduce ()
  "Creature reproduction.")
