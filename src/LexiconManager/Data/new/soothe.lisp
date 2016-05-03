;;;;
;;;; W::soothe
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::soothe
   (SENSES
    ((LF-PARENT ont::subduing)
     (example "music soothes the savage beast")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (LF-PARENT ONT::evoke-comfort)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

