;;;;
;;;; W::redefine
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::redefine
   (SENSES
    ((LF-PARENT ONT::classify)
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090501 :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "redefine this")
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-OPTIONAL-TEMPL)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::classify)
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "redefine this" "this was redefined by him")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

