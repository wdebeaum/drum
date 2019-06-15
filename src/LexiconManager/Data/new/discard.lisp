;;;;
;;;; W::discard
;;;;

(define-words :pos W::v 
 :words (
  (W::discard
   (SENSES
    ((LF-PARENT ONT::discard)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::from))))
     (EXAMPLE "discard the message")
     (meta-data :origin task-learning :entry-date 20050826 :change-date 20090529 :comments nil)
     )
    )
   )
))

