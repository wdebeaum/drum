;;;;
;;;; W::bcc
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::bcc
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     (example "what is the bcc address")
     (LF-PARENT ONT::copy)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::bcc
    (SENSES
     ((lf-parent ont::sendcopy)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (templ agent-recipient-affected-templ (xp (% W::PP (W::ptype w::on))))
     (example "bcc him on that")
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     )
     )
    )
))

