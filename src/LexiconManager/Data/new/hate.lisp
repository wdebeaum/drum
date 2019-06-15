;;;;
;;;; W::hate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::hate
   (SENSES
    ((LF-PARENT ONT::disliking)
     (meta-data :origin calo :entry-date 20050425 :change-date 20090511 :comments projector-purchasing)
     (SEM (F::Aspect F::indiv-level))
     (example "I hate oranges")
     (TEMPL experiencer-neutral-templ)
     )
    ((LF-PARENT ONT::disliking)
     (meta-data :origin calo :entry-date 20050425 :change-date 20090511 :comments projector-purchasing)
     (SEM (F::Aspect F::indiv-level))
     (example "I hate to dance")
     (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL)
     ;(TEMPL experiencer-action-objcontrol-templ)
     )
    )
   )
))

