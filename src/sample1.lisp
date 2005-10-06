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

(make-new-world)

(defvar *Creature1* (make-instance 'Creature ))

(schedule #'(lambda () (format T "About to escape:") (signal 'escape)) *world* 1)
(schedule #'(lambda () (format T "DIE! ") (signal 'dead :creature *Creature1*)) *world* 2)

(advance-time *world*)