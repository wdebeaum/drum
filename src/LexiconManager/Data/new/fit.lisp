;;;;
;;;; W::fit
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  (W::fit
   (wordfeats (W::morph (:forms (-vb) :ing W::fitting :past W::fit)))
   (SENSES
    ((EXAMPLE "The ball fits under the table" "will it fit in the truck")
     (LF-PARENT ONT::could-be-at)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-location-TEMPL)
     )
    ((EXAMPLE "He fit it under the table")
     (LF-PARENT ONT::put)
     (TEMPL agent-affected-xp-TEMPL)
     (meta-data :origin gloss :entry-date 20110127 :change-date nil :comments Jansen)
     )
    )
   )
))

