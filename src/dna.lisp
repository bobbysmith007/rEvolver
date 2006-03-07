(in-package :rEvolver)

;;(?A -> (list ?Rewrite))
;;(?Rewrite -> ({attrib-block-1} [cond] {attrib-block-2} : (?RightPart)))
;;(?RightPart -> name ?rewrite-name ?rewrite-name => (write-part))
;;(?rewrite-name -> )

(defparameter *depth-bound* 10
  "the depth at which tree generation terminates")

;;; A grammer specification for the DNA language
(defparameter Creature-DNA-1

  '((?Start -> ?Expression)
    (?Expression ->
     (;function defintion and application 
      (lambda ?Symbol ?Expression =>  (lambda (type . lambda)))
      (gamma  (?Expression (type . lambda))
			   ?Expression => gamma)
      ;list
      (cons ?Expression ?Expression => (cons (type . list)))
      (cdr (?Expression (type . 'list)) => cdr)
      (car (?Expression (type . 'list)) => car)

      
      ;boolean 
      (or ?Expression ?Expression => or)
      (if ?Expression ?Expression ?Expression => if)
      (not ?Expression => (not (type . 'boolean)))
		     
      ;predicates
      (equal? ?Expression ?Expression => (equal (type . boolean)))
      (type? ?Expression ?Type =>  (eq (type . boolean)))
      
;Environment
;(move (?Expression (type . 'node) (return . )) => move)
;(energy? (?Expression (type . 'node) (return . boolean)) => energy?)
;(feed  (?Expression (type . 'node)) => feed)
;(look (?Expression (type . 'node)) => look)  

      (?Terminal)
      
		    
      ))
    (?Type ->
     ((node => node)
      (function => function)
      (list => list)
      (atom => atom)
      (number => number)
      ))
    
    (?Symbol ->
     ((gensym => gensym)
      (*gened-sym* => *gened-sym*)))
    
    (?Terminal ->
     ((nil => nil)
      (t => t)
      (*gened-sym* => *gened-sym*)
      ;Environment
      (move => move)
      (energy? => energy?)
      (feed => feed)
      
      (?Type)))
    ))



(defun process-grammer-definition (grammer)
  "Removes the arrows and makes them nice nested a-lists.
TODO: This should probably actually make some sort of struct rather than rediculously nested lists
   (?name . ( (rewrite) (rewrite) ...)) 
   (rewrite . (right-part . write-part)) "
  
  (mapcar
   (lambda (grammer-spec)
     (cons (car grammer-spec)
	   (let ((rewrites-into (caddr grammer-spec)))
	   (if (listp rewrites-into)
	       (mapcar
		(lambda (right-part)
		  (let ((loc (position '=> right-part )))
		    (if loc
			(cons (subseq right-part 0 loc)
			      (subseq right-part (1+ loc) (length right-part)))
			right-part )))
		rewrites-into)
	       rewrites-into))))
   grammer)
  )


(defun rewrite-token? (symbol)
  (let ((symbol (rewrite-node-name symbol)))
    (let ((pos (position #\? (rewrite-node-name (symbol-name symbol)))))
      (if (and pos (= 0 pos))
	T nil))))

(defun right-part (rewrites-to) (car rewrites-to))

(defun get-rewrite-tokens (rewrites-to)
  (let ((right-part (right-part rewrites-to)))
    (cond ((listp right-part)
	   (remove-if-not
	    #'rewrite-token?
	    right-part))
	  ((rewrite-token? right-part) (list right-part))
	  (t (error "cant figure out how to get rewrite tokens of:~a" rewrites-to))
	  )))

(defun write-part (rewrites-to)
  (let ((write-part (cadr rewrites-to)))
    (if (listp write-part)
	(car write-part)
	write-part)))

(defun possibilities (processed-grammer rewrite-name) 
  (cdr (assoc rewrite-name processed-grammer)))

(defun rewrite-node-name (val)
  "gets the name of the rewrite node"
  (if (consp val)
      (car val)
      val))

(defun pick-possibility (list current-depth)
  (when list
	(adwutils::get-random-element list)))

(defun get-child-nodes (processed-grammer rewrite-tokens current-depth)
  (mapcar
   (lambda (new-expansion)
     (generate-tree processed-grammer (rewrite-node-name new-expansion) (1+ current-depth)))
   rewrite-tokens))


(defun generate-tree (processed-grammer rewrite-name &optional (current-depth 0))
  (declare (optimize (debug 3))
	   (special *depth-bound*))
  
    (let* (;get all of the rewrite possibilities of the rule
	   (possibilities (if (= *depth-bound* current-depth)
			      (possibilities processed-grammer '?Terminal)
			      (possibilities processed-grammer rewrite-name)))	 
	   (chosen (pick-possibility possibilities current-depth))
	   
	   ;; Get all the expansions for this rewrite
	   (rewrite-tokens (get-rewrite-tokens chosen))
	   ;; what tree-node to build
	   (write-part (write-part chosen))
	   (write-name (rewrite-node-name write-part))
	   
	   ;; get all the subtrees neccessary for this
	   (child-nodes (get-child-nodes processed-grammer rewrite-tokens current-depth)))
      
      (format T "~%------~%chosen:~a ~%" chosen)      
      (format T "rewrite-tokens:~a ~%" rewrite-tokens)
      (format T "write-part:~a ~%" write-part)
      (format T "children:~a ~%" child-nodes)
      (format T "current-depth:~a~%" current-depth)
      
	(if child-nodes
	    (if write-name
		(cons write-name child-nodes)
		(if (< 1 (length child-nodes))
		    child-nodes
		    (car child-nodes)))
	    write-name)
      ))