(in-package :generator)

;;(?A -> (list ?Rewrite))
;;(?Rewrite -> ({attrib-block-1} [cond] {attrib-block-2} : (?RightPart)))
;;(?RightPart -> name ?rewrite-name ?rewrite-name => (write-part))
;;(?rewrite-name -> )

(defparameter +depth-bound+ 10
  "the depth at which tree generation terminates")

(defun process-grammar-definition (grammar)
  "Removes the arrows and makes them nice nested a-lists.
TODO: This should probably actually make some sort of struct rather than rediculously nested lists
   (?name . ( (rewrite) (rewrite) ...)) 
   (rewrite . (right-part . write-part)) "
  
  (mapcar
   (lambda (grammar-spec)
     (cons (car grammar-spec)
	   (let ((rewrites-into (caddr grammar-spec)))
	   (if (listp rewrites-into)
	       (mapcar
		(lambda (right-part)
		  (let ((loc (position 'dna::=> right-part )))
		    (if loc
			(cons (subseq right-part 0 loc)
			      (subseq right-part (1+ loc) (length right-part)))
			right-part )))
		rewrites-into)
	       rewrites-into))))
   grammar)
  )


(defun rewrite-token? (symbol)
  (let ((symbol (rewrite-node-name symbol)))
    (let ((pos (position #\? (rewrite-node-name (symbol-name symbol)))))
      (if (and pos (= 0 pos))
	T nil))))

(defun right-part (rewrites-to) (car rewrites-to))

(defun get-rewrite-tokens (rewrites-to)
  (declare (optimize (debug 3)))
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
    write-part))

(defun possibilities (processed-grammar rewrite-name) 
  (cdr (assoc rewrite-name processed-grammar)))

(defun rewrite-node-name (val)
  "gets the name of the rewrite node"
  (if (consp val)
      (car val)
      val))

(defun pick-possibility (list current-depth)
  (declare (ignore current-depth))
  (when list
	(random-elt list)))

(defun get-child-nodes (processed-grammar rewrite-tokens current-depth symbol-table)
  (mapcar
   (lambda (new-expansion)
     (let ((new-sub-tree
	    (generate-tree (1- current-depth)
			   processed-grammar
			   (rewrite-node-name new-expansion)
			   symbol-table)))
       
       (cond ((eq new-sub-tree 'dna::gensym)
	      (car (push (gensym) symbol-table)))
	     ((eq new-sub-tree 'dna::*gened-sym*)
	      ;; When we have gensym
	      (or (random-elt symbol-table)
		  (when (eq '?symbol new-expansion)
		      (gensym))))
	     (T new-sub-tree))))
   rewrite-tokens))

(defun depth-first-expression-replace (list symbol-list value-list)
  "Replaces tokens from symbol-list with values from value-list in the list
  (depth-first-expression-replace '(gamma (gamma cons ?expression) ?expression) '(?Expression ?Expression) '(a b))
  dna::=> '(gamma (gamma cons a) b) "
  (labels ((depth-first-expression-replace (list)
	     (unless (null list)
	       (let ((elem (car list))
		     (rest (rest list)))
		 (cond ((listp elem)
			(cons (depth-first-expression-replace elem)
			      (depth-first-expression-replace rest)))
		       ((eq (car symbol-list) elem)
			(pop symbol-list)
			(cons (pop value-list)
			      (depth-first-expression-replace rest)))
		       (T
			(cons elem
			      (depth-first-expression-replace rest))))))))
    
    (depth-first-expression-replace list)))
;(depth-first-expression-replace '(gamma (gamma cons ?expression) ?expression) '(?Expression ?Expression) '((a) (b)))


(defun generate-tree (&optional (current-depth +depth-bound+)
				(processed-grammar revolver:creature-dna)
				(rewrite-name '?Start)
				(symbol-table nil))
  (declare (optimize (debug 3)))
  
    (let* (;get all of the rewrite possibilities of the rule
	   (possibilities (if (= 0 current-depth)
			      (possibilities processed-grammar '?Terminal)
			      (possibilities processed-grammar rewrite-name)))
	   (chosen (pick-possibility possibilities current-depth))
	   
	   ;; Get all the expansions for this rewrite
	   (rewrite-tokens (get-rewrite-tokens chosen))

	   ;; what tree-node to build
	   (write-part (write-part chosen))
	   	   
	   ;; get all the subtrees neccessary for this
	   (child-nodes (get-child-nodes processed-grammar rewrite-tokens current-depth symbol-table) )
	   (write-tree (or (if (atom write-part) write-part
			       (depth-first-expression-replace write-part rewrite-tokens child-nodes))
			   (if  (listp child-nodes)
				(car child-nodes)
				child-nodes))))
      
;      (format T "~%------~%chosen:~a ~%" chosen)      
;      (format T "rewrite-tokens:~a ~%" rewrite-tokens)
;      (format T "write-part:~a ~%" write-part)
;      (format T "write-tree:~a ~%" write-tree)
;      (format T "children:~a ~%" child-nodes)
;      (format T "current-depth:~a~%" current-depth)
      
	write-tree
      ))
