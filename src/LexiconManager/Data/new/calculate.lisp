;;;;
;;;; W::calculate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::calculate
    (wordfeats (W::morph (:forms (-vb) :nom W::calculation)))
   (SENSES
    ((LF-PARENT ONT::calculation)
     (meta-data :origin trips :entry-date 20060414 :change-date 20090522 :comments nil :wn ("calculate%2:31:00"))
     (templ agent-theme-xp-templ)
     (example "he calculated the sum")
     )
    )
   )
))

