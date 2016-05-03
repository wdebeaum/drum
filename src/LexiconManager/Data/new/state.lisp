;;;;
;;;; W::STATE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::STATE
   (SENSES
    ((meta-data :origin plow :entry-date 20060803 :change-date nil :comments nil :wn ("state%1:15:01"))
     (LF-PARENT ONT::state)
     (example "new york state")
     )
    ((LF-PARENT ONT::status)
     (templ other-reln-templ)
     (EXAMPLE "the state of operation, state of health, financial state")
     (meta-data :origin task-learning :entry-date 20050818 :change-date 20081112 :wn ("state%1:03:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::state
   (SENSES
    ((LF-PARENT ONT::assert)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     (EXAMPLE "he stated that he couldn't come")
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil)
     )
    (;;(LF-PARENT ONT::talk)
     (lf-parent ont::assert)
     (example "he stated his name/purpose")
     (TEMPL AGENT-THEME-XP-TEMPL)
     )
    )
   )
))

