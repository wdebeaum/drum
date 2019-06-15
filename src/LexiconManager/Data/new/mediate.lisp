;;;;
;;;; W::mediate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::mediate
   (wordfeats (W::morph (:forms (-vb) :nom w::mediation)))
   (SENSES
    ((lf-parent ont::control-manage)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))
