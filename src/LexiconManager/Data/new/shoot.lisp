;;;;
;;;; W::SHOOT
;;;;

(define-words :pos W::n
 :words (
  (W::SHOOT
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::shoot
   (wordfeats (W::morph (:forms (-vb) :ing W::shooting :past W::shot)))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s14)
     (LF-PARENT ONT::MOVE-quickly)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AFFECTED-TEMPL)
     (example "the truck shot over to beahan")
     )
    ((LF-PARENT ONT::evoke-injury)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

