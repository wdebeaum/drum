;;;;
;;;; W::fix
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::fix (W::up))
   (SENSES
    ((LF-PARENT ONT::REPAIR)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "he fixed up the apartment")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::fix
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090522 :comments nil :vn ("price-54.4") :wn ("fix%2:36:01"))
     ;(LF-PARENT ONT::control-manage)
     (LF-PARENT ONT::manipulate)
     (example "fix the price")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090504 :comments nil :vn ("preparing-26.3-1"))
     (LF-PARENT ONT::create-by-cooking)
     (example "fix dinner")
     (TEMPL AGENT-AFFECTEDR-XP-TEMPL) ; like prepare
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::REPAIR)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "he fixed the truck with the wrench")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

