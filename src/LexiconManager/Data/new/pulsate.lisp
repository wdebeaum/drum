;;;;
;;;; W::pulsate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::pulsate
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::pulse)
     (example "the blood pulsated in his veins")
     (TEMPL affected-templ)
     (meta-data :origin cardiac :entry-date 20081215 :change-date nil :comments LM-vocab)
     )
    )
   )
))

