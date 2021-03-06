(in-package :rEvolver)

(defclass node ()
  ((energy :initarg :energy :accessor energy :initform 0)
   (creatures :initform nil :accessor creatures-of)
   (rEvolver-map :initarg :rEvolver-map :reader rEvolver-map))
  (:documentation "A location on a map."))

(defclass rEvolver-map ()
  ((free-energy :initform 0 :accessor free-energy)
   (creature-count :initform 0 :accessor creature-count))
  (:documentation "Generic map interface."))

(defclass 2d-array-map (rEvolver-map)
  ((x-size :initarg :x-size :accessor x-size)
   (y-size :initarg :y-size :accessor y-size)
   (array)))

(defclass 2d-node (node)
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)))

;(defmethod print-object ((location location) stream)
;  (format stream "<#Location (~a,~a) with Energy(~a) Creature(~a) >"
;	  (x location)
;	  (y location)
;	  (energy location)
;	  (length (creatures location))))

(defgeneric random-node (map)
  (:documentation "Get a random node on a map."))

(defgeneric adjacent-nodes-of (node)
  (:documentation "What nodes are adjacent to the given node."))

(defgeneric adjacent-p (node1 node2)
  (:documentation "Are two nodes adjacent. I.E. is there an arc from node1 to node2."))

(defgeneric take-energy (node &optional amount)
  (:documentation "Remove all the energy from a node, returning how much there was.")
  (:method ((node node) &optional (amount (energy node)))
	   (let ((amount (min (energy node) amount)))
	     (setf (energy node) (- (energy node) amount))
	     (decf (free-energy (revolver-map node)) amount)
	     amount)))

(defgeneric add-energy (node amount)
  (:method ((node node) amount)
	   "This function will return the energy that it actually added to the node
 (in case we try to exceed the max)"
	   (let* ((new-node-energy (min (+ (energy node) amount)
					(node-energy-max *simulation*)))
		  (added-energy
		   (- new-node-energy (energy node))))
	     (setf (energy node) new-node-energy)
	     (incf (free-energy (revolver-map node)) added-energy)
	     added-energy)))

(defmethod initialize-instance :after ((m 2d-array-map) &key)
  (with-slots (array x-size y-size) m
    (setf array (make-array (list x-size y-size)
			    :initial-element nil
			    :adjustable nil))))

(defmethod map-nodes (fn (map 2d-array-map))
  (let ((list ()))
    (loop for x from 0 to (1- (x-size map))
	  do (loop for y from 0 to (1- (y-size map))
		   do (push (funcall fn (find-node-xy map x y)) list)))
    (reverse list)
    )
  )

(defmethod node-energy% ((node node))
  (round (* 100 (/ (energy node) (node-energy-max *simulation*)))))

(defmethod creatures ((map 2d-array-map))
  (kmrcl:flatten (map-nodes (lambda (node)
			      (creatures-of node))
			    map)))


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


(defmethod adjacent-nodes-of ((node 2d-node))
  (with-slots (x y revolver-map) node 
    (list (find-node-xy revolver-map (1- x) y)
	  (find-node-xy revolver-map (1+ x) y)
	  (find-node-xy revolver-map x (1- y))
	  (find-node-xy revolver-map x (1+ y))
	  (find-node-xy revolver-map (1+ x) (1+ y))
	  (find-node-xy revolver-map (1+ x) (1- y))
	  (find-node-xy revolver-map (1- x) (1+ y))
	  (find-node-xy revolver-map (1- x) (1- y)))))

(defmethod adjacent-p ((node1 2d-node) (node2 2d-node))
  (and (<= (abs (- (x node1) (x node2))) 1)
       (<= (abs (- (y node1) (y node2))) 1)))