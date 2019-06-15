;;;;
;;;; W::penetrate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::penetrate
   (SENSES
    ((EXAMPLE "the vehicle penetrated the enclosure")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Penetrate)
     ;(LF-PARENT ONT::entering)
     (LF-PARENT ONT::penetrate)
     ;(templ agent-neutral-xp-templ)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

