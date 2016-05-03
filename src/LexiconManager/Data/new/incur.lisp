;;;;
;;;; W::incur
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::incur
   (wordfeats (W::morph (:forms (-vb) :ing W::incurring :past W::incurred :nom w::incursion)))
   (senses
    
    ((lf-parent ont::incur-inherit-receive) ;; 20120524 GUM change new parent
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     (example "a cost usually incurred with projectors")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-affected-templ)
     )
    )
   )
))

