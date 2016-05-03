;;;;
;;;; W::activate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::activate
    (wordfeats (W::morph (:forms (-vb) :nom w::activation)))
   (SENSES
    ((EXAMPLE "activate all alerting") 
     (LF-PARENT ont::start)
     (templ agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )

    ((EXAMPLE "The army activates") 
     (LF-PARENT ont::start)
     (templ affected-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )

    )
   )
))

