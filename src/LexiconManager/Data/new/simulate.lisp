;;;;
;;;; w::simulate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::simulate
    (wordfeats (W::morph (:forms (-vb) :nom w::simulation)))
    (senses
     ((LF-PARENT ONT::imitate-simulate)
      (example "simulate the procedure")
      (templ agent-neutral-templ)
      (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil)
      )
     ))
))

