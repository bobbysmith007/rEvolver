(in-package :rEvolver)

(defclass location ()
  ((energy :initarg :energy :accessor energy :initform 0)
   (creatures :initform nil :accessor creatures)
   (x :initarg :x :reader x)
   (y :initarg :y :reader y)
   (rEvolver-map :initarg :rEvolver-map :reader rEvolver-map))
  (:documentation "A location on a map."))

(defmethod print-object ((location location) stream)
  (format stream "<#Location (~a,~a) with Energy(~a) Creature(~a) >"
	  (x location)
	  (y location)
	  (energy location)
	  (length (creatures location))))

(defclass rEvolver-map ()
  ((x-size :initarg :x-size :accessor x-size)
   (y-size :initarg :y-size :accessor y-size))
  (:documentation "Generic map interface."))

(defgeneric inspect-map (rEvolver-map x y)
  (:documentation "Get a location describing a point on the map."))

(defgeneric east-of (location)
  (:method ((l location))
	   (inspect-map (revolver-map l) (1+ (x l)) (y l))))

(defgeneric west-of (location)
  (:method ((l location))
	   (inspect-map (revolver-map l) (1- (x l)) (y l))))

(defgeneric north-of (location)
  (:method ((l location))
	   (inspect-map (revolver-map l) (x l) (1+ (y l)))))

(defgeneric south-of (location)
  (:method ((l location))
	   (inspect-map (revolver-map l) (x l) (1- (y l)))))

(defmethod drop-random-energy ((m rEvolver-map) frequency energy-to-add-max/spot )
  (dotimes (n (* frequency (x-size m) (y-size m)))
	(let ((l (inspect-map m (random (x-size m)) (random (y-size m))))
	      (e (random energy-to-add-max/spot)))
	  (setf (energy l) (+ e (energy l)))))
  m)

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
	  (setf (aref array x y) (make-instance 'location :x x :y y :rEvolver-map m))
	  (aref array x y)))))
