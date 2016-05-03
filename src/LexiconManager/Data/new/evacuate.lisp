;;;;
;;;; W::evacuate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::evacuate
   (SENSES
    ((LF-PARENT ONT::evacuate)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "evacuate delta")
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090529 :comments nil :vn ("banish-10.2") :wn ("evacuate%2:38:00" "evacuate%2:38:01"))
     (LF-PARENT ONT::empty)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "evacuate the people")
     )
    )
   )
))

