;;;;
;;;; W::zero
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
((W::zero w::in)
   (SENSES
    ((LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "zero in on the problem")
     (TEMPL agent-neutral-xp-templ (xp (% W::pp (W::ptype W::on))))
     (meta-data :origin calo-ontology :entry-date 20060201 :change-date nil :comments meeting-understanding)
     )
    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::zero
	   (senses
	    ((lf-parent ont::zero-val)
	     (meta-data :origin beetle :entry-date 20081112 :change-date 20090915 :comments pilot5)
	     (example "a zero voltage means that terminals are connected")
	     ;; this sentence is possible but unlikely, hence a low preference
	     ;; note that this is a true adjective: "a 5 voltage" is not possible
	     (preference 0.97)
	     )
	    ))	  
))

