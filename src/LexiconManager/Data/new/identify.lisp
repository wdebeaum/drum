;;;;
;;;; W::identify
;;;;

(define-words :pos W::v 
 :words (
	 (W::identify
	  (wordfeats (W::morph (:forms (-vb) :nom w::identification)))
	  (SENSES
	   ((LF-PARENT ONT::identify)
	    (meta-data :origin lou :entry-date 20040716 :change-date 20041116 :comments lou-demo)
	    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	    (example "identify this (as a mine)")
	    (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL)
	    )
	   
	   ((LF-PARENT ONT::identify)
	    (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
	    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	    (example "this was identified by him")
	    (TEMPL AGENT-NEUTRAL-XP-TEMPL)
	    )
	   )
	  )
	 ))

