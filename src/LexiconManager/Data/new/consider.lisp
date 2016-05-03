;;;;
;;;; W::consider
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::consider
   (wordfeats (W::morph (:forms (-vb) :past W::considered :ing W::considering :nom w::consideration)))
   (SENSES
    ((LF-PARENT ONT::SCRUTINY)
     (example "consider this option")
     (TEMPL agent-theme-xp-templ)
     )
    ((LF-PARENT ONT::believe)
     (example "he considered the speaker a genius")
     (TEMPL neutral-neutral-adj-predicate-optional-templ)
     (meta-data :origin calo :entry-date 20060124 :change-date nil :comments meeting-understanding)
     )
    )
   )
))

