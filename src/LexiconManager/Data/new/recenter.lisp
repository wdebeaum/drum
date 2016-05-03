;;;;
;;;; W::recenter
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::recenter
    (wordfeats (W::morph (:forms (-vb) :ing w::recentering :past W::recentered)))   
   (SENSES
    ((LF-PARENT ONT::arranging)
     (SEM (F::Aspect F::bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-THEME-XP-TEMPL)
     (meta-data :origin task-learning :entry-date 20050916 :change-date 20090507 :comments nil)
     (example "recenter the image within the window")
     )
    )
   )
))

