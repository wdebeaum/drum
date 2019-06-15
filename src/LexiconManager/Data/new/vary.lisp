;;;;
;;;; W::vary
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	 (W::vary
	  (wordfeats (W::morph (:forms (-vb) :nom w::variation)))
	  (SENSES
	   ((meta-data :origin calo :entry-date 20040915 :change-date 20090504 :comments caloy2)
	    (LF-PARENT ONT::fluctuate)
	    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	    )
	   ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
	    (LF-PARENT ONT::fluctuate)
	    (example "they vary in size")
	    (TEMPL AFFECTED-FORMAL-XP-OPTIONAL-TEMPL  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
	    )
	   ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
	    (LF-PARENT ONT::fluctuate)
	    (example "they vary between 5 and 10 inches")
	    (TEMPL AFFECTED-FORMAL-XP-ADVBL-TEMPL)
	    )
	   )
	  )
	 ))

