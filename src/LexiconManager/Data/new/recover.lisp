;;;;
;;;; w::recover
;;;;

(define-words :pos W::v 
 :words (
  (w::recover
   (wordfeats (W::morph (:forms (-vb) :ing w::recovering :past W::recovered :nom w::recovery)))
   (SENSES
    ((lf-parent ont::acquire)
     (example "recover an accidentally deleted slide")
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil :vn ("obtain-13.5.2") :wn ("recover%2:40:00"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

