;;;;
;;;; W::love
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::love
   (SENSES
    ((LF-PARENT ONT::appreciate)
     (meta-data :origin calo :entry-date 20050425 :change-date 20090508 :comments projector-purchasing)
     (SEM (F::Aspect F::indiv-level))
     (example "I love oranges")
     (TEMPL experiencer-neutral-xp-templ)
     )
    ((LF-PARENT ONT::appreciate)
     (meta-data :origin calo :entry-date 20050425 :change-date 20090508 :comments projector-purchasing)
     (SEM (F::Aspect F::indiv-level))
     (example "I love to dance")
     (TEMPL experiencer-action-objcontrol-templ)
     )
    )
   )
))

