;;;;
;;;; W::mediate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::mediate
   (wordfeats (W::morph (:forms (-vb) :nom w::mediation)))
   (SENSES
    ((lf-parent ont::control-manage)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))
