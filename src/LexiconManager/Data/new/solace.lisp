;;;;
;;;; W::solace
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::solace
    (wordfeats (W::morph (:forms (-vb) :nom w::solace)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("solace%2:37:00"))
     ;(LF-PARENT ont::experiencer-obj)
     (LF-PARENT ONT::EVOKE-RELIEF)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    )
   )
))

