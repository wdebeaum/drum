;;;;
;;;; W::posit
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::posit
   (wordfeats (W::morph (:forms (-vb) :past w::posited :ing w::positing)))
   (SENSES
    (
     (LF-PARENT ONT::ASSUME)
     (SEM (F::Aspect F::stage-level))
     (TEMPL experiencer-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::ASSUME)
     (SEM (F::Aspect F::stage-level))
     (TEMPL neutral-neutral-xp-templ)
     )
    )
   )
))
