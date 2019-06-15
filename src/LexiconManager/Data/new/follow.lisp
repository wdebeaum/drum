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
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  ((W::follow (W::up))
   (wordfeats (W::morph (:forms (-vb) :nom (w::follow w::up))))
   (SENSES
    ((LF-PARENT ONT::activity-ongoing)
     (TEMPL AGENT-NEUTRAL-XP-TEMPL)
     (example "I followed up the leads.")
     )
    ((LF-PARENT ONT::activity-ongoing)
     (TEMPL AGENT-NEUTRAL-XP-TEMPL (xp (% W::pp (W::ptype (? xx W::on)))))
     (example "I followed up on the leads.")
     )
    )
   )
))
