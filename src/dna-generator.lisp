(in-package :generator)

(deflogger logger ()
  :level +error+
  :appender (make-instance 'brief-stream-log-appender :stream t))

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
		  (when (eq 'dna::?symbol new-expansion)
		      (gensym))
		  'dna:nil))
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
			      (possibilities processed-grammar 'dna::?Terminal)
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

      (logger.dribble "chosen:~s " chosen)      
      (logger.dribble "rewrite-tokens:~s " rewrite-tokens)
      (logger.dribble "write-part:~s " write-part)
      (logger.dribble "write-tree:~s " write-tree)
      (logger.dribble "children:~s " child-nodes)
      (logger.dribble "current-depth:~s~%" current-depth)
      
	write-tree
      ))


(defvar *left-chance* +depth-bound+ )
(defvar *right-chance* +depth-bound+ )
(defvar *stop-chance* +depth-bound+ )
(defparameter *mutation-rate* .01)
(defparameter *value-mutation-rate* .01)
(defparameter *mutation-depth* (/ +depth-bound+ 2))


(defun make-path-decision ( tree )
  "returns the location of the replace ment (:root 0 :left 1 :right 2)"
  (if (atom tree) 0
      (let* ((sum (+ *left-chance* *right-chance* *stop-chance*))
	     (rand (random (* 1.0 sum))))
;	(format T "sum: ~a - rand: ~a  ~a  ~a  ~a~%" sum rand  *left-chance* *right-chance* *stop-chance*)
	(cond ((< 0 rand *left-chance*)  1)
	      ((< *left-chance* rand (+ *left-chance* *right-chance*))
	       2)
	      (T 0)))))

(defun replace-random-subtree (tree replace-tree)
  "Replaces a random (sub)tree with the replacement-tree"
    (labels ((rec-replace-random-subtree (sub-tree &optional (parent nil) (location 0))
	       (let ((loc (make-path-decision sub-tree)))
;		 (format T "parent: ~a~%replace: ~a~%location: ~a~%-------~%" parent replace-tree location)
		 (cond
		   ((and (null parent)
			 (= 0 loc)) ;we just selected the root
		    replace-tree)
		   ((= 0 loc) ;we chose to stop at this node
		    (setf (nth location parent) replace-tree)
		    tree)
		   (T (rec-replace-random-subtree (nth loc sub-tree) sub-tree loc))
		   
		   ))))
      (rec-replace-random-subtree tree)))

(defun maybe-mutate-tree (tree &optional (mutation-rate *mutation-rate*) (mutation-depth *mutation-depth*))
  (let ((maybe (random 1.0)))
    (if (not (< maybe mutation-rate))
	tree
	(let ((new-tree (generate-tree mutation-depth)))
	  (replace-random-subtree tree new-tree)))
    ))

(defun maybe-mutate-value (val &optional (mutation-rate *mutation-rate*) (value-mutation-rate  *value-mutation-rate*))
  (let ((maybe (random 1.0)))
    (if (not (< maybe mutation-rate))
	val
	(let* ((a (* val  value-mutation-rate))
	       (b (* 2 a))
	       (delta-rand (- (random b) a)))
	  (+ val delta-rand)))))