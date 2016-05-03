;;;;
;;;; W::experience
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::experience
   (wordfeats (W::morph (:forms (-vb) :nom W::experience)))
   (SENSES
    ((LF-PARENT ONT::have-experience)
     (example "he is experiencing a headache")
     (meta-data :origin cardiac :entry-date 20080217 :change-date nil :comments nil)
     (TEMPL neutral-pred-xp-templ)
     )
    )
   )
))

