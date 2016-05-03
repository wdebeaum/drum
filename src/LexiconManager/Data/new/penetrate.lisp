;;;;
;;;; W::penetrate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::penetrate
   (SENSES
    ((EXAMPLE "the vehicle penetrated the enclosure")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Penetrate)
     (LF-PARENT ONT::entering)
     (templ agent-neutral-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

