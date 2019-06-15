;;;;
;;;; W::merge
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::merge
   (SENSES
    ((EXAMPLE "merge this cell with the next cell")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil)
     )
    ; need more senses
    ((EXAMPLE "the truck merged into the fast lane")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060323 :change-date nil :comments nil)
     )
    ;; need this template to go through the adjectival passive rule
    ((EXAMPLE "the merged examples")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

