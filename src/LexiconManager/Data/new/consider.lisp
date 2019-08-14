;;;;
;;;; W::consider
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::consider
   (wordfeats (W::morph (:forms (-vb) :past W::considered :ing W::considering :nom w::consideration)))
   (SENSES
    ((LF-PARENT ONT::SCRUTINY)
     (example "consider this option")
     (TEMPL agent-neutral-xp-templ)
     )
    ((LF-PARENT ONT::believe)
     (example "he considered the speaker a genius")
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-2-XP-3-XP2-ADJP-OPTIONAL-TEMPL)
     (meta-data :origin calo :entry-date 20060124 :change-date nil :comments meeting-understanding)
     )
    )
   )
))

