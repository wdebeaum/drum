;;;;
;;;; W::recharge
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::recharge
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1"))
     (LF-PARENT ONT::evoke-excitement)
     (example "the pep talk recharged him")
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
     (EXAMPLE "recharge the battery")
     (LF-PARENT ONT::fill-container)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     )
    )
   )
))

