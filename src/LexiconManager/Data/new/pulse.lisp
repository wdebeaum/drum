;;;;
;;;; W::pulse
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::pulse
   (wordfeats (W::morph (:forms (-vb) :nom W::pulse)))
   (SENSES
    ((LF-PARENT ONT::rhythmic-motion)
     (example "the blood pulsed in his veins")
     (TEMPL affected-templ)
     )
    )
   )
))


(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pulse
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("pulse%1:28:00"))
     (LF-PARENT ONT::pulse)
     )
    )
   )
))
