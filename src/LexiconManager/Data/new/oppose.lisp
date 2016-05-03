;;;;
;;;; W::oppose
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::oppose
   (SENSES
    ((meta-data :origin calo :entry-date 20050905 :change-date 20090508 :comments projector-purchasing)
     ;;(LF-PARENT ONT::contest)
     (lf-parent ont::contest-deny-oppose-protest) ;; 20120523 GUM change new parent
     (example "I oppose the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

