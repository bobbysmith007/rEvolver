(in-package :rEvolver)

(defparameter +possible-creatures+ '())
(defparameter +movement-energy-ratio+ 1/10)


(defclass creature ()
  ((energy :accessor energy :initform 0 :initarg :energy)
   (location :accessor location :initarg :location)
   (world :reader world :initarg :world)
   (decision-fn :accessor decision-fn)))

(define-condition dead ()
  ((creature :initarg :creature :accessor creature)))

(defmethod use-energy ((creature creature) amount)
  (when (>= 0 (decf (energy creature) amount))
    (signal 'dead :creature creature)))

(defmethod add-creature ((creature creature) (location location))
  "add a creature to a location."
  (push creature (creatures location))
  (setf (location creature) location))

(defmethod remove-creature ((creature creature) (location location))
  "Take a creature out of a location."
  (delete creature (creatures location) :test #'eq)
  (setf (location creature) nil))



(defun move (direction)
  "Move the creature."
  (declare (special *current-creature*))
   ;get a new lexical binding for the creature
  (let ((creature *current-creature*))
    (format-log "Scheduling a move for: ~a~%" creature)
    (schedule (domove creature direction)
	      (world creature)
	      1)
    (format-log "About to escape...~%")
    (signal 'escape)))

(defun domove (creature direction)
  #'(lambda ()
      (format-log "Starting to move: ")
      (let ((l (location creature))
	    (dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
	(format-log "moving: (location ~a) (dirfn ~a)~%" l dirfn)
	(remove-creature creature l)
	(format-log "Moving: Left ~a..." l)
	;;before we add them to the new location use the energy (which might kill them)
	(use-energy creature (* (energy creature) +movement-energy-ratio+))
	(format-log "Creature depleted: ~a~%" creature)
	(let ((l (funcall dirfn l)))
	  (format-log "Found the next square: ~a~%" l)
	  (add-creature creature l)
	  (format-log "Now in ~a.~%" l)))
      (schedule #'(lambda () (funcall (decision-fn creature))) (world creature) 1)))

;(defmacro define-creature-op (name lambda-list &key documentation energy action ticks)
;  "Ease in the creation of operations a creature can perform.
;	For now I'm thinking this will mean having it specialized on a creature."
;    ;;get the name of the symbol in the creatures package
;  (let ((fn-name (intern (string name) :rEvolver.creature))
;	(creature (gensym "creature")))
;    `(defun ,name (,creature ,@lambda-list)
;      ,documentation
;      (schedule #'(lambda ()
;		    ,@action
;		    (use-energy ,creature ,energy )
;		    )
;       (world ,creature)
;       ,ticks))))


;;;; Define-creature-method is a macro to create a method
;;;; specialized on a type of creature
 
  
;(define-creature-op look ((direction nil))
;  "Examine the location the creature is currently standing at if the location
;	is null, or look in the specified direction.")

;(define-creature-op move (direction)
;  :documentation "Move the creature in a specified direction."
;  :ticks 10
;  :action (let ((l (location creature))
;		(dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
;	    (remove-creature creature l) 
;	    (add-creature creature (funcall dirfn l))
;	    creature)
;  :energy (* (energy creature) +movement-energy-ratio+))


;(define-creature-op feed ()
;  "Feed from the energy source at the current location.")

;(define-creature-op reproduce ()
;  "Creature reproduction.")



(defmethod print-object ((cr creature) stream)
  (format stream "(Creature :Energy ~a)" (energy cr)))