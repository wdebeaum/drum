;;;;
;;;; W::converge
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::converge
   (SENSES
    ; they converged on the house
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("focus-87"))
     ;(LF-PARENT ONT::physical-scrutiny)
     ;(TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::on)))) ; like concentrate,focus
     (LF-PARENT ONT::MEET)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL (xp (% w::pp (w::ptype w::on))))  
     )

    ; they converged
    ((LF-PARENT ONT::meet)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     )
    )
   )
))

