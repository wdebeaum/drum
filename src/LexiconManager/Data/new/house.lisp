;;;;
;;;; W::HOUSE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::HOUSE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("house%1:06:00"))
     (LF-PARENT ont::lodging)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::house
   (SENSES
    ((LF-PARENT ONT::CONTAINMENT)
      (EXAMPLE "The hangar houses an airplane")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("fit-54.3" "pocket-9.10") :wn ("house%2:41:00"))
     )
    )
   )
))

