;;;;
;;;; W::interpret
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	 (W::interpret
	  (wordfeats (W::morph (:forms (-vb) :past w::interpreted :pastprt w::interpreted :ing w::interpreting :nom w::interpretation)))
	  (SENSES
	   ((LF-PARENT ONT::classify)
	    (meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090501 :comments Interpret :vn ("characterize-29.2"))
	    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	    (example "I interpret this message as a warning")
	    (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL)
	    )
	  
	   ((LF-PARENT ONT::classify)
	    (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
	    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	    (example "this was interpreted by him")
	    (TEMPL AGENT-NEUTRAL-XP-TEMPL)
	    )
	   )
	  )
	 ))

