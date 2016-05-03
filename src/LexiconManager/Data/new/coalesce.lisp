;;;;
;;;; W::coalesce
;;;;

(define-words :pos W::v 
 :words (
   (W::coalesce
   (SENSES
    ((LF-PARENT ONT::coalesce)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
     (meta-data :origin gloss-training :entry-date 20100217 :change-date nil :comments nil)
      )
     ((LF-PARENT ONT::coalesce)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     (meta-data :origin gloss-training :entry-date 20100217 :change-date nil :comments nil)
      )
    )
   )
))

