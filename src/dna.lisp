(in-package :rEvolver)

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
(defparameter Creature-DNA-1
  '((?Start -> ?Expression)
    (?Expression -> (
		    ;function defintion and application 
		    (lambda ?Symbol ?Expression => lambda $ ((type . lambda)))
		    (gamma ?Expression $ ((type . lambda))
			   ?Expression => gamma)
		    ;list
		    (cons ?Expression ?Expression => cons $ ((type . 'list)))
		    (cdr ?Expression $ ((type . 'list)) => cdr)
		    (car ?Expression $ ((type . 'list)) => car)


		    ;boolean 
		    (or ?Expression ?Expression => or)
		    (if ?Expression ?Expression ?Expression => if)
		    (not ?Expression => not $ ((type . 'boolean)))
		     
		    ;predicates
		    (equal? ?Expression ?Expression => equal $ ((type . 'boolean)))
		    (type? ?Expression ?Types => type? $ ((type . 'boolean)))

		    ;Environment 
		    (move ?Expression $ ((type . 'node)) => move)
		    (energy? ?Expression $ ((type . 'node)) => energy?)
		    (feed ?Expression $ ((type . 'node)) => feed)

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

(defun process-grammer-definition (grammer)
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
  (if (atom rewrites-to) rewrites-to
      (let ((fun (car rewrites-to))
	    (left-to-process (cdr rewrites-to)))
	(loop for elem = (car left-to-process)
	      for next-elem = (or (cadr left-to-process) 'end)
	      when elem collect
	      (if (eq '$ next-elem) ;;if we have constraints get them
		  (prog1
		    (cons elem
			  (caddr left-to-process))
		    (setf left-to-process (cdddr left-to-process)))
		  (prog1
		    elem
		    (setf left-to-process (cdr left-to-process))))
	      into defs
	      unless elem do (return (cons fun defs)))
	    )))

(defun generate-tree (processed-grammer rewrite-rule &optional output)
  (let* ((possibilities (assoc rewrite-rule processed-grammer))
	 (chosen (pick-possibility possibilities))
	 (rewrite-tokens (get-rewrite-tokens chosen)))
    
    
    )
  )