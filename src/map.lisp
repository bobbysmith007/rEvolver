(in-package :rEvolver)

(defclass location ()
  ((energy :initarg :energy :accessor energy :initform 0)
   (creatures :initform nil :reader creatures))
  (:documentation "A location on a map."))

(defclass rEvolver-map ()
  ((x-size :initarg :x-size :initform 40 :accessor x-size)
   (y-size :initarg :y-size :initform 40 :accessor y-size))
  (:documentation "Generic map interface."))

(defgeneric inspect-map (rEvolver-map x y)
  (:documentation "Get a location describing a point on the map."))

(defgeneric east-of ( rEvolver-map location)
  (:method ((m rEvolver-map) (l location))
	   (inspect-map m (1+ x) y)))

(defgeneric west-of ( rEvolver-map location)
  (:method ((m rEvolver-map) (l location))
	   (inspect-map m (1- x) y)))

(defgeneric north-of ( rEvolver-map location)
  (:method ((m rEvolver-map) (l location))
	   (inspect-map m x (1+ y))))

(defgeneric south-of ( rEvolver-map location)
  (:method ((m rEvolver-map) (l location))
	   (inspect-map m x (1- y))))

(defmethod drop-random-energy ((m rEvolver-map) frequency energy-to-add-max/spot )
  (dotimes (n (* frequency (x-size m) (y-size m)))
	(let ((l (inspect-map m (random (x-size m)) (random (y-size m))))
	      (e (random energy-to-add-max/spot)))
	  (setf (energy l) (+ e (energy l))))))

(defclass 2d-array-map (rEvolver-map)
  ((array)))

(defmethod initialize-instance :after ((m 2d-array-map) &key)
  (with-slots (array x-size y-size) m
    (setf array (make-array (list x-size y-size)))))

(defmethod inspect-map ((m 2d-array-map) x y)
  (with-slots (x-size y-size array) m
    (let ((x (mod x x-size))
	  (y (mod y y-size)))
      (if (null (aref array x y))
	  (setf (aref array x y) (make-instance 'location))
	  (aref array x y)))))
