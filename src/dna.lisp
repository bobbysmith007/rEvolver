(CL:in-package :dna)

;;; A grammar specification for the DNA language
(CL:setf revolver:Creature-DNA
  (revolver::process-grammar-definition
   '((revolver::?Start -> ((?Expression)))
     (?Expression ->
      (;function defintion and application
       
       (lambda ?Symbol ?Expression
	       =>
	       (lambda ?Symbol ?Expression))
       
       (gamma ?Expression ?Expression
	       =>
	       (gamma ?Expression ?Expression))
	;list
       (cons ?Expression ?Expression
	     =>
	     (gamma (gamma cons ?Expression) ?Expression))
       
       (cdr ?Expression
	    =>
	    (gamma cdr ?Expression))
       
       (car ?Expression
	    =>
	    (gamma car ?Expression))
       
       ;boolean 
       (or ?Expression ?Expression
	   =>
	   (gamma (gamma or ?Expression) ?Expression))

       ;;arithmetic
       (- ?Expression
	    =>
	    (gamma - ?Expression))
       (< ?Expression ?Expression
	   =>
	   (gamma (gamma < ?Expression) ?Expression))
       (+ ?Expression ?Expression
	   =>
	   (gamma (gamma + ?Expression) ?Expression))
       
       (if ?Expression ?Expression ?Expression
	   =>
	   (gamma (gamma (gamma (gamma if ?Expression)
				(lambda nil ?Expression))
			 (lambda nil ?Expression))
		  nil))
	   
       (not ?Expression
	    => 
	    (gamma not ?Expression))
       
	;predicates
       (equal? ?Expression ?Expression
	    =>
	    (gamma (gamma equal ?Expression) ?Expression))
       
       (type? ?Expression ?Type
	      =>
	      (gamma (gamma eq ?Expression) ?Type))
       
       ;Environment
       (move ?Expression
	     => (gamma move ?expression))
       (energy ?Expression
	     => (gamma energy ?expression))
       (energy => (gamma energy nil))
       (feed ?Expression
	     => (gamma feed ?expression))
;       (creatures ?expression
;	     => (gamma creatures ?expression))

       
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
       (zero => 0)
       (*gened-sym* => *gened-sym*)
       (Me => Me)
      ;Environment
       (look-at
	=>
	(gamma look-at nil))

       (asexually-reproduce
	=>
	(gamma asexually-reproduce nil))
       
       (?Type))))))

