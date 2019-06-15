;;;;
;;;; W::allot
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::allot
   (wordfeats (W::morph (:forms (-vb) :past W::allotted)))
   (SENSES
    ((lf-parent ont::assign)
     (TEMPL AGENT-AFFECTED-TEMPL)
     (example "allot the resources to them")
     (meta-data :origin calo-ontology :entry-date 20051208 :change-date nil :comments action-subclasses)
     )
    )
   )
))

