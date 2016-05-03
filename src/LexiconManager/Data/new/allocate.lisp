;;;;
;;;; W::allocate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::allocate
   (SENSES
    ((lf-parent ont::assign)
     (templ agent-affected-recipient-alternation-templ)
     (example "allocate the resources to them")
     (meta-data :origin calo-ontology :entry-date 20051208 :change-date nil :comments action-subclasses)
     )
    )
   )
))

