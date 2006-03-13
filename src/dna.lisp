(CL:in-package :dna)

;;; A grammar specification for the DNA language
(CL:setf revolver:Creature-DNA
  (generator:process-grammar-definition
   '((generator:?Start -> ((?Expression)))
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
       (move
	=>
	(gamma move nil))
       (energy?
	=>
	(gamma energy? nil))
       (feed
	=>
	(gamma feed nil))
       
       (?Type))))))

