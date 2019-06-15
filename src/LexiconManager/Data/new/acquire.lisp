;;;;
;;;; w::acquire
;;;;

(define-words :pos W::v 
 :words (
  (w::acquire
   (SENSES
    ((lf-parent ont::acquire)
     (example "acquire the product (from them)")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil :vn ("obtain-13.5.2-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::from))))
     )
    )
   )
))

