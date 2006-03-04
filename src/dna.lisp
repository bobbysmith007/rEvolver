(in-package :rEvolver)

(defparameter Creature-DNA-1
  '((?Start -> ?Expression)
    (?Expression -> (
		    ;function defintion and application 
		    (lambda ?Symbol ?Expression =>  (lambda (type . lambda)))
		    (gamma  (?Expression (type . lambda))
			   ?Expression => gamma)
		    ;list
		    (cons ?Expression ?Expression => (cons (type . 'list)))
		    (cdr (?Expression (type . 'list)) => cdr)
		    (car (?Expression (type . 'list)) => car)


		    ;boolean 
		    (or ?Expression ?Expression => or)
		    (if ?Expression ?Expression ?Expression => if)
		    (not ?Expression => (not (type . 'boolean)))
		     
		    ;predicates
		    (equal? ?Expression ?Expression => (equal (type . 'boolean)))
		    (type? ?Expression ?Types =>  (type? (type . 'boolean)))

		    ;Environment 
		    (move (?Expression (type . 'node)) => move)
		    (energy? (?Expression (type . 'node)) => energy?)
		    (feed  (?Expression (type . 'node)) => feed)

		    ;Symbols
		    (nil => nil)
		    (t => t)
		    (?Type ) 
		    )
     
     )
    (?Type -> ((node => node)
	       (function => function)
	       (list => list)
	       (atom => atom)
	       (number => number)
	       )
     )
    (?Symbol -> ((gensym => 'gensym)
		 (*gened-sym* => '*gened-sym*)))
    ))

; lambda
; gamma

; not
; ;;and
; or
; if
; equal
; <

; cons
; cdr
; car

; atom?
; lambda?
; list?
; null?
; node?
; 

; nil

; (move &optional <node>)
; ;;(look &optional <node>); (energyp &optional <node>) ;;energy at optional node or current node 
; (feed) ;; get energy from right here

;;; A grammer specification for the DNA language
;;; Expression : (alist of Constrainer notes)  are constrainer 


(defun process-grammer-definition (grammer)
  "Removes the arrows and makes them nice nested a-lists"
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

(defun get-rewrite-tokens (rewrites-to)
  ;;the cdr because the first token is always the write part for now
  (if (listp (car rewrites-to))
      (cdr (car rewrites-to))
      (car rewrites-to))
  )

(defun write-part (rewrites-to)
  (cadr rewrites-to)
  )

(defun pick-possibility (list)
  (when list
    (adwutils::get-random-element list)))

(defun possibilities (processed-grammer rewrite-name) 
  (cdr (assoc rewrite-name processed-grammer)))

(defun generate-tree (processed-grammer rewrite-name)
  (declare (optimized (debug 3) ))
  (let* ((possibilities (possibilities processed-grammer rewrite-name))
	 (chosen (pick-possibility possibilities))
	 (rewrite-tokens (get-rewrite-tokens chosen))
	 ;; what tree-node to build
	 (write-part (write-part chosen))
	 (child-nodes (mapcar
		       (lambda (new-expansion)
			 (if (listp new-expansion)
			     (generate-tree processed-grammer (car new-expansion))
			     (generate-tree processed-grammer new-expansion)))
		       rewrite-tokens)))
    
    (format T "rewrite-tokens:~a ~%" rewrite-tokens)
    (format T "write-part:~a ~%" write-part)
    
    (let ((write-part (if (listp write-part)
			  (car write-part)
			  write-part)))
      (if child-nodes
	    (cons write-part child-nodes)
	    write-part))
    ))