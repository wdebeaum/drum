;;;;
;;;; W::distribute
;;;;

(define-words :pos W::v 
 :words (
  (W::distribute
   (wordfeats (W::morph (:forms (-vb) :nom W::distribution)))
   (SENSES
    ((lf-parent ont::assign)
     (templ agent-affected-plural-recipient-optional-templ)
     (example "distribute gifts to them")
     (meta-data :origin task-learning :entry-date 20050818 :change-date 20090501 :comments nil)
    )
    ((lf-parent ont::assign)
     (templ agent-affected-mass-recipient-optional-templ)
     (example "distribute the software to them")
     (meta-data :origin task-learning :entry-date 20050818 :change-date 20090501 :comments nil)
    )
   )
  )
))

