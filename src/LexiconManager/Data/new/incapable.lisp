;;;;
;;;; W::incapable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::incapable
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090731 :wn ("incapable%3:00:00" "incapable%5:00:00:incompetent:00") :comments caloy3)
     (EXAMPLE "He is incapable of reason")
     (LF-PARENT ONT::unable)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::of))))
     (SEM (F::GRADABILITY +))
     )
    )
   )
))

