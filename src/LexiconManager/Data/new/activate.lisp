;;;;
;;;; W::activate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::activate
    (wordfeats (W::morph (:forms (-vb) :nom w::activation :agentnom w::activator)))
   (SENSES
    ((EXAMPLE "activate the alert systems") 
     (LF-PARENT ont::start)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    ((EXAMPLE "The army activated.")
     (LF-PARENT ont::start)
     (templ affected-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))
