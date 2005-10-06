(in-package :rEvolver)

(defparameter +initial-creature-count+ 50
  "The number of random creatures to initially populate the world with.")

(defparameter +world-size+ 20)

(defparameter +energy-frequency+ .3)
(defparameter +energy-max+ 100)
(defparameter +drop-energy-turns+ 10)
(defvar *world* '())

(defun make-new-world ()
  (let ((m (make-instance '2d-array-map :x-size +world-size+ :y-size +world-size+)))
    (drop-random-energy m .3 100)
    (setf *world* (make-instance 'world :map m))))


(defun random-location ()
  (let ((m (world-map *world*)))
    (inspect-map m (random (x-size m)) (random (y-size m)))))

(make-new-world)

(defvar *Creature1* ())
(setf *Creature1* (make-instance 'Creature :energy 128 :world *world* :location (random-location)))

(setf (decision-fn *creature1*)
      (let ()
      #'(lambda ()
	  (let ((*current-creature* *Creature1*))
	    (declare (special *current-creature*))
	    (move 'east)))))

(schedule #'(lambda () (funcall (decision-fn *creature1*))) (world *creature1*) 1)