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

; atomp
; funp
; listp

; nil

; (move &optional <node>)
; ;;(look &optional <node>); (energyp &optional <node>) ;;energy at optional node or current node 
; (feed) ;; get energy from right here

;;; A grammer specification for the DNA language
;;; Expression : (alist of Constrainer notes)  are constrainer 
(defparameter Creature-DNA-1
  '(
    (?Expression -> (
		    (lambda ?Symbol ?Expression => lambda)
		    (gamma ?Expression $((type . lambda))
			   ?Expression => gamma)

		    (cons ?Expression ?Expression => cons)
		    (cdr ?Expression $ ((type . 'list)) => cdr)
		    (car ?Expression $ ((type . 'list)) => car)

		    (tag ?Expression $ ((doc . "Connects a type to a value to a val"))
			 ?Expression => tag)
		    (type ?Expression => type)
		    (value ?Expression => value)
		     
		    (or ?Expression ?Expression => or)
		    (if ?Expression ?Expression ?Expression => if)
		    (not ?Expression => not)
		    ;predicates
		    (equal? ?Expression ?Expression => equal)

		    (move ?Expression $ ((type .  (equal? null ?value))) => move)
		    (energy? ?Expression $ ((type . node)) => energy?)
		    (nil => nil)
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
	   (mapcar
	    (lambda (right-part)
	      (let ((loc (position '=> right-part )))
		(if loc
		    (cons (subseq right-part 0 loc)
			  (subseq right-part (1+ loc) (length right-part)))
		    right-part )))
	    (caddr grammer-spec))))
   grammer)
  )

(defun create-interpretter (processed-grammer))