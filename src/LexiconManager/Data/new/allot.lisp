;;;;
;;;; W::allot
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::allot
   (wordfeats (W::morph (:forms (-vb) :past W::allotted)))
   (SENSES
    ((lf-parent ont::assign)
     (templ agent-affected-recipient-alternation-templ)
     (example "allot the resources to them")
     (meta-data :origin calo-ontology :entry-date 20051208 :change-date nil :comments action-subclasses)
     )
    )
   )
))

