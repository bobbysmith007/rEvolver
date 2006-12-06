(in-package :revolver.web-serv)

(defcomponent frame (simple-window-component )
  ((show-map :component (show-map)))
  (:render (self)
	   (render (slot-value self 'show-map))))

(defentry-point "show-map.ucw" (:application *revolver-application*)
    ()
  (let (;(container (make-instance ))
	))
    ;(add-component container )
    (call 'frame
	  :title (format nil "rEvolver map on ~a" (machine-instance))
	  ))