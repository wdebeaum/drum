;;;;
;;;; W::DIFFICULTY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DIFFICULTY
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("difficulty%1:04:00"))
     (LF-PARENT ONT::trouble)
     (TEMPL mass-PRED-TEMPL)
     )
    ((LF-PARENT ONT::trouble)
     (example "he has difficulty doing that")
     (meta-data :origin calo :entry-date 20031130 :change-date nil :wn ("difficulty%1:04:00") :comments caloy2)
     (TEMPL mass-PRED-assoc-with-TEMPL  (XP (% W::vp (W::vform W::ing))))
     )
    ((meta-data :origin domain-reorganization :entry-date 20170810 :change-date nil :comments nil :wn ("difficulty%1:07:00"))
     (LF-PARENT ONT::difficulty-scale)
     (EXAMPLE "They agreed about the difficulty of the climb")
     )
    )
   )
))

