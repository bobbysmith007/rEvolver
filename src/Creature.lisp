(in-package :rEvolver)

(defparameter +possible-creatures+ '())
(defparameter +movement-energy-ratio+ 1/10)


(defclass creature ()
  ((energy :accessor energy :initform 0 :initarg :energy)
   (node :accessor node :initarg :node)
   (world :reader world :initarg :world)
   (decision-fn :accessor decision-fn)))

(defmethod die ((creature creature))
  (when (node creature)))

(define-condition dead ()
  ((creature :initarg :creature :accessor creature)))

(defmethod use-energy ((creature creature) amount)
  (when (>= 0 (decf (energy creature) amount))
    (signal 'dead :creature creature)))

(defmethod add-creature ((creature creature) (node node))
  "add a creature to a node."
  (push creature (creatures node))
  (setf (node creature) node))

(defmethod remove-creature ((creature creature) (node node))
  "Take a creature out of a node."
  (delete creature (creatures node) :test #'eq)
  (setf (node creature) nil))


(defun move (direction)
  "Move the creature."
  (declare (special *current-creature*))
   ;get a new (unique) lexical binding for the creature.
  (let ((creature *current-creature*))
    
    (schedule (domove creature direction)
	      (world creature)
	      1)
    (signal 'escape)))

(defun domove (creature direction)
  #'(lambda ()
      (let ((l (node creature))
	    (dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
	(remove-creature creature l)
	;;before we add them to the new node use the energy (which might kill them)
	(use-energy creature (* (energy creature) +movement-energy-ratio+))
	(let ((l (funcall dirfn l)))
	  (add-creature creature l)))
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
;  "Examine the node the creature is currently standing at if the node
;	is null, or look in the specified direction.")

;(define-creature-op move (direction)
;  :documentation "Move the creature in a specified direction."
;  :ticks 10
;  :action (let ((l (node creature))
;		(dirfn (symbol-function (intern (concatenate 'string (string direction) "-OF")))))
;	    (remove-creature creature l) 
;	    (add-creature creature (funcall dirfn l))
;	    creature)
;  :energy (* (energy creature) +movement-energy-ratio+))


;(define-creature-op feed ()
;  "Feed from the energy source at the current node.")

;(define-creature-op reproduce ()
;  "Creature reproduction.")



(defmethod print-object ((cr creature) stream)
  (format stream "(Creature :Energy ~a)" (energy cr)))