;;;;
;;;; W::reorder
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::reorder
   (wordfeats (W::morph (:forms (-vb) :past W::reordered :ing w::reordering)))
   (SENSES
    ((lf-parent ont::arranging)
     (example "reorder them")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    ((lf-parent ont::arranging)
     (example "reorder them by departure date")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-theme-TEMPL (xp (% W::PP (W::ptype W::by))))
     (meta-data :origin plow :entry-date 20070928 :change-date nil :comments plow-travel)
     )
    )
   )
))

