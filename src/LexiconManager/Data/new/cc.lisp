;;;;
;;;; W::cc
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::cc
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     (example "what is the cc address")
     (LF-PARENT ONT::copy)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
    (W::cc
    (SENSES
     ((lf-parent ont::sendcopy)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (templ agent-recipient-affected-templ (xp (% W::PP (W::ptype w::on))))
     (example "cc him on that")
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     )
     )
    )
))

