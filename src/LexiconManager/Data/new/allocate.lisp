;;;;
;;;; W::allocate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::allocate
   (wordfeats (W::morph (:forms (-vb) :nom w::allocation)))
   (SENSES
    ((lf-parent ont::assign)
     (TEMPL AGENT-AFFECTED-TEMPL)
     (example "allocate the resources to them")
     (meta-data :origin calo-ontology :entry-date 20051208 :change-date nil :comments action-subclasses)
     )
    )
   )
))

