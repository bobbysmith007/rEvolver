(in-package :rEvolver.map)

(defclass node ()
  ((energy :initarg :energy :accessor energy :initform 0)
   (creatures :initform nil :accessor creatures)
   (rEvolver-map :initarg :rEvolver-map :reader rEvolver-map))
  (:documentation "A location on a map."))


;(defmethod print-object ((location location) stream)
;  (format stream "<#Location (~a,~a) with Energy(~a) Creature(~a) >"
;	  (x location)
;	  (y location)
;	  (energy location)
;	  (length (creatures location))))

(defclass rEvolver-map ()
  ()
  (:documentation "Generic map interface."))

(defgeneric random-node (map)
  (:documentation "Get a random node on a map."))

(defgeneric drop-random-energy (map frequency energy-to-add-max/spot)
  (:documentation "Drop a bunch of energy on a map"))


(defclass 2d-array-map (rEvolver-map)
  ((x-size :initarg :x-size :accessor x-size)
   (y-size :initarg :y-size :accessor y-size)
   (array)))

(defmethod initialize-instance :after ((m 2d-array-map) &key)
  (with-slots (array x-size y-size) m
    (setf array (make-array (list x-size y-size)
			    :initial-element nil
			    :adjustable nil)))
  )


(defclass 2d-node (node)
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)))

(defgeneric find-node-xy (map x y)
  (:method  ((m 2d-array-map) x y)
	    (with-slots (x-size y-size array) m
	      (let ((x (mod x x-size))
		    (y (mod y y-size)))
		(symbol-macrolet ((aref (aref array x y)))
		  (if (null aref)
		      (setf aref (make-instance '2d-node :x x :y y :rEvolver-map m))
		      aref))))))


(defmethod random-node ((m 2d-array-map))
  (find-node-xy m (random (x-size m)) (random (y-size m))))

(defmethod drop-random-energy ((m 2d-array-map) frequency energy-to-add-max/spot )
  (dotimes (n (* frequency (x-size m) (y-size m)))
	(let ((l (random-node m))
	      (e (random energy-to-add-max/spot)))
	  (setf (energy l) (+ e (energy l)))))
  m)


