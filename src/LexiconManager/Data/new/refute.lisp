;;;;
;;;; w::refute
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
	      :words (
		      (w::refute
		       (wordfeats (W::morph (:forms (-vb) :nom w::refutation)))
		       (senses
			((LF-parent ont::contest)
			 (Example "He refutes the findings")
			 (TEMPL agent-formal-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
			 (meta-data :origin calo :entry-date 20050509 :change-date 20090508 :comments projector-purchasing) 
			 )
			(
			 (LF-PARENT ONT::REFUTE)
			 (example "The result refuted the hypothesis")
			 (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
			 (TEMPL neutral-neutral-xp-templ)
			 )

			(
			 (LF-PARENT ONT::REFUTE)
			 (example "The result refuted that the gene activates the protein")
			 (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
			 (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
			 )
			))
))

