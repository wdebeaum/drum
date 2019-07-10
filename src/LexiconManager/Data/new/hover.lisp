;;;;
;;;; W::hover
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::hover
   (wordfeats (W::morph (:forms (-vb) :past W::hovered :ing w::hovering)))
   (SENSES
     ;((LF-PARENT ONT::FLY)
     ;(SEM (F::Aspect F::bounded) (F::Time-span F::extended))
;      )
    ;;;; hover over the landscape 
    ((LF-PARENT ONT::hang-out)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

