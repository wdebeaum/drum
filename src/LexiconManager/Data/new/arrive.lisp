;;;;
;;;; W::arrive
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::arrive
   (wordfeats (W::morph (:forms (-vb) :nom W::arrival :nomsubjpreps (w::of) :nomobjpreps - :agentnom w::arrival)))
   (SENSES
    #|
    ((LF-PARENT ONT::ARRIVE)
     (example "the truck arrived in/at delta from rochester")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-result-xp-TEMPL )
     )
    |#
    ((LF-PARENT ONT::ARRIVE)
     (example "the truck arrived")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-TEMPL )
     )
    )
   )
))
