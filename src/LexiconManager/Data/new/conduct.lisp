;;;;
;;;; w::conduct
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (w::conduct
   (senses
    ((EXAMPLE "conduct an investigation")
     (LF-PARENT ont::execute)
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-neutral-xp-templ)
     )
    #|
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::guiding)
     (example "team alpha will conduct the activity")
     (TEMPL agent-affected-xp-templ)
     )
    |#
    )
   )
))

