;;;;
;;;; W::follow
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :tags (:base500)
 :words (
  (W::follow
   (SENSES
    ((LF-PARENT ONT::FOLLOW-PATH)
     (example  "I follow truck B" "The truck followed the road to Avon")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    ((LF-PARENT ONT::Compliance)
     (example "follow this procedure")
     (templ agent-theme-xp-templ)
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil)
     )
    )
   )
))

