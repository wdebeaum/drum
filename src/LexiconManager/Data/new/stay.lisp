;;;;
;;;; W::stay
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
	      :tags (:base500)
	      :words (
		      (W::stay
		       (SENSES
    ;;;; I've simplified this - the one new sense covers all the examples we had
			#|
			((LF-PARENT ONT::STAY)
			(SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
			(TEMPL agent-theme-complex-subjcontrol-templ)
			(example "the truck stayed at the depot")
			)
			|#
			((LF-PARENT ONT::STAY)
			 (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
			 (TEMPL affected-TEMPL)
			 (example "they stayed")
			 )
			))
		      ))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::stay w::in w::touch)
   (SENSES
    ((LF (W::GOODBYE))
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CLOSE))
     )
    )
   )
))

