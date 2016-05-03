;;;;
;;;; W::fly
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::fly
   (wordfeats (W::morph (:forms (-vb) :past W::flew :pastpart W::flown)))
   (SENSES
       ;;;; Fly the cargo to Avon
    ((LF-PARENT ONT::FLY)
     (SEM (F::ASPECT F::BOUNDED))
     )
    ;;;; FLY to Avon
    ((LF-PARENT ONT::FLY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

