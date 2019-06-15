;;;;
;;;; w::surge
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (w::surge
   (wordfeats (W::morph (:forms (-vb) :nom w::surge)))
   (SENSES
    ((lf-parent ont::move-quickly)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::Atomic))
     (TEMPL affected-TEMPL)
     )
    )
   )
))

