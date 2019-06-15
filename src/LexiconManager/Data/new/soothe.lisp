;;;;
;;;; W::soothe
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::soothe
   (SENSES
    ((LF-PARENT ont::evoke-relief)
     (example "music soothes the savage beast")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (LF-PARENT ONT::evoke-relief)
     (example "This pill will soothe your headache")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

