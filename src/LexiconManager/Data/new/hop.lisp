;;;;
;;;; W::hop
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::hop
   (SENSES
    ;;;; hop on the highway
    ((LF-PARENT ONT::jump)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ)
     )
    )
   )
  ))

