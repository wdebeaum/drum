;;;;
;;;; W::pulsate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::pulsate
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::rhythmic-motion)
     (example "the blood pulsated in his veins")
     (TEMPL affected-templ)
     (meta-data :origin cardiac :entry-date 20081215 :change-date nil :comments LM-vocab)
     )
    )
   )
))

