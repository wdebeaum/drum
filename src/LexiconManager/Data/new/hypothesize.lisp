;;;;
;;;; W::hypothesize
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::hypothesize
   (wordfeats (W::morph (:forms (-vb) :nom W::hypothesis)))
   (SENSES
    (
     (LF-PARENT ONT::HYPOTHESIZE)
     (SEM (F::Aspect F::stage-level))
     (example "We hypothesize the gene activates the protein")
     (TEMPL experiencer-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::HYPOTHESIZE)
     (SEM (F::Aspect F::stage-level))
     (example "We hypothesize this theory")
     (TEMPL experiencer-neutral-xp-templ)
     )

    )
   )
))

