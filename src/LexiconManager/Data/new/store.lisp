;;;;
;;;; W::store
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::store
   (SENSES
    ((LF-PARENT ONT::commercial-facility)
     (meta-data :origin caloy2 :entry-date 20050404 :change-date nil :wn ("store%1:06:00") :comments projector-purchasing)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::store
   (SENSES
    ((meta-data :origin calo :entry-date 20031219 :change-date nil :comments s11)
     (EXAMPLE "The truck stored the cargo")
     (LF-PARENT ONT::CONTAINMENT)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20040423 :change-date nil :comments s11)
     ;(LF-PARENT ONT::duplicate)
     ;(TEMPL agent-neutral-xp-templ)
     (LF-PARENT ONT::put-away)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "he stored the data on the computer")
     )
    )
   )
))

