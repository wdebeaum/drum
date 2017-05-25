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
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("keep-15.2") :wn ("store%2:40:00" "store%2:40:02"))
     (LF-PARENT ONT::retain)
     (TEMPL agent-affected-xp-templ) ; like keep
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20031219 :change-date nil :comments s11)
     (EXAMPLE "The truck stored the cargo")
     (LF-PARENT ONT::CONTAINMENT)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-templ)
     )
    ((meta-data :origin calo :entry-date 20040423 :change-date nil :comments s11)
     ;(LF-PARENT ONT::duplicate)
     ;(TEMPL agent-neutral-xp-templ)
     (LF-PARENT ONT::put-away)
     (TEMPL agent-affected-xp-templ)
     (example "he stored the data on the computer")
     )
    )
   )
))

