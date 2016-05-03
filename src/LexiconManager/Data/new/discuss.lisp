;;;;
;;;; W::discuss
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::discuss
   (wordfeats (W::morph (:forms (-vb) :nom w::discussion)))
   (SENSES
    ((lf-parent ont::discuss)
     (example "he discussed it [with her]")
     (TEMPL agent-theme-affected-optional-templ)
     )
    ))
  ))

