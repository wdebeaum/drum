;;;;
;;;; w::zoom
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  (w::zoom
   (wordfeats (W::morph (:forms (-vb) :nom w::zoom)))
   (SENSES
    ((lf-parent ont::move-quickly)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-TEMPL)
     (example "he zoomed across the room")
     )
    ((lf-parent ont::move-quickly)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     (example "zoom the camera in/out")
     (meta-data :origin coordops :entry-date 20070514 :change-date nil :comments nil)
     )
    )
   )
))

