;;;;
;;;; W::posit
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::posit
   (wordfeats (W::morph (:forms (-vb) :past w::posited :ing w::positing)))
   (SENSES
    (
     (LF-PARENT ONT::ASSUME)
     (SEM (F::Aspect F::stage-level))
     (TEMPL experiencer-formal-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::ASSUME)
     (SEM (F::Aspect F::stage-level))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    )
   )
))
