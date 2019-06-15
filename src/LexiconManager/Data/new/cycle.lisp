;;;;
;;;; W::cycle
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	 (W::cycle
	  (wordfeats (W::morph (:forms (-vb) :nom w::cycle)))
	  (SENSES
	   ((meta-data :origin calo :entry-date 20040915 :change-date 20090504 :comments caloy2)
	    (LF-PARENT ONT::fluctuate)
	    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	    )
	   ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
	    (LF-PARENT ONT::fluctuate)
	    (example "they vary between 5 and 10 inches")
	    (TEMPL AFFECTED-FORMAL-XP-ADVBL-TEMPL)
	    )
	   )
	  )
	 ))

