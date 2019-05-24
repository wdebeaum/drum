;;;;
;;;; W::loathe
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::loathe
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("admire-31.2") :wn ("loathe%2:37:00"))
     (LF-PARENT ONT::disliking)
;     (TEMPL experiencer-action-objcontrol-templ) ; like suffer
     (TEMPL experiencer-action-SUBJCONTROL-TEMPL)
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::disliking)
     (meta-data :origin calo :entry-date 20050425 :change-date 20090508 :comments projector-purchasing)
     (SEM (F::Aspect F::indiv-level))
     (example "I loathe oranges")
     (TEMPL experiencer-neutral-xp-templ)
     )
    )
   )
))

