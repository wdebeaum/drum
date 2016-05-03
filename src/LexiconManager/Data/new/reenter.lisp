;;;;
;;;; W::reenter
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::reenter
   (SENSES
    ((EXAMPLE "I am reentering the canyon")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     (LF-PARENT ONT::entering)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-neutral-xp-TEMPL)
     )
    ((EXAMPLE "reenter the title in the textbox")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     (LF-PARENT ONT::put)
     (templ agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

