;;;;
;;;; W::dine
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::dine
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060119 :change-date nil :comments caloy3)
     (LF-PARENT ONT::consume)
     (example "he dines (on bread)")
     (SEM (F::ASPECT F::DYNAMIC))
     (templ agent-affected-optional-templ  (xp (% W::pp (W::ptype W::on))))
     )
    )
   )
))

