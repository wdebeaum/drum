;;;;
;;;; W::retrieve
;;;;

(define-words :pos W::v 
 :words (
  (W::retrieve
   (wordfeats (W::morph (:forms (-vb) :nom W::retrieval)))
   (SENSES
    ((lf-parent ont::retrieve)
     (example "retrieve the information from the website")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt w::on W::at W::from)))))
     (meta-data :origin plow :entry-date 20060523 :change-date nil :comments pq0390)
     )
    )
   )
))

