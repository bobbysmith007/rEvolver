(in-package :revolver.web-serv)

(defcomponent show-map ()
  ()
  (:default-initargs)
  (:render (self)
	   
	   (let* ((2d-map (revolver::revolver-map *world*))
		  (x-size (revolver::x-size 2d-map))
		  (y-size (revolver::y-size 2d-map))
		  (node-count (* x-size y-size)))
	     (macrolet ((tr (&rest cols)
			  `(<:tr ,@(mapcar #'(lambda (c) `(<:td (<:as-html ,c))) cols))))
	       (<:p
		(<:table
		 (tr "World:" (inspect-anchor self *world*))
		 (tr "Tick:" (tick-number *world*))
		 (tr "Creature Count:" (creature-count (revolver-map *world*)))
		 (tr "Creatures/Node:" (float (/ (creature-count (revolver-map *world*))
						 node-count)))
		 (tr "Animation Record:" (and *golem* (animation-count *golem*)))
		 (tr "Population:" (revolver::population-infusions *world*))
		 (tr "Repopulation:" (repopulation-infusions *world*))
		 (tr "Free Energy:" (free-energy (revolver-map *world*)))
		 (tr "Free Energy/Node:" (truncate
					  (/ (free-energy (revolver-map *world*))
					     node-count)))))
	       (<:style ".bar {height: 10px;  background-color:green;}
.map td {border: 1px solid #888; width: 50px; height 50px; margin:0; padding:0;}")
	       (<:p
		(<:table :cellpadding 0 :cellspacing 0 :class "map"
 
		 (loop for y from 0 to (1- y-size)
		       do
		       (<:tr
			(loop for x from 0 to (1- x-size)
			      for node = (revolver::find-node-xy 2d-map x y)
			      do
			      (<:td
				    (<:div :class "bar" :style (format nil "width: ~a%;" (revolver::node-energy% node)))
				    (let ((len (length (revolver::creatures-of node))))
				      (if (> len 0)
					  (inspect-anchor self node  len)
					  (<:as-is len)))))))))
	       ))))


