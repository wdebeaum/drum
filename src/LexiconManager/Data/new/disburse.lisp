;;;;
;;;; W::disburse
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::disburse
   (SENSES
    ((LF-PARENT ONT::commerce-pay)
     (TEMPL agent-cost-templ)
     (example "he disbursed 5 dollars")
     (meta-data :origin "wordnet-3.0" :entry-date 20090501 :change-date nil :comments nil)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090501 :change-date nil :comments nil)
     (LF-PARENT ONT::commerce-pay)
     (TEMPL AGENT-COST-neutral-TEMPL (xp (% W::PP (W::ptype W::for))))
     (example "he disbursed 5 dollars for it")
     )
    ((LF-PARENT ONT::commerce-pay)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::atomic))
     (example "disburse the money to me")
     (meta-data :origin "wordnet-3.0" :entry-date 20090501 :change-date nil :comments nil)
     (TEMPL AGENT-cost-goal-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))

