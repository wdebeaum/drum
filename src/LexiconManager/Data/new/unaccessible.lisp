;;;;
;;;; W::unaccessible
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::unaccessible
   (SENSES
    ((lf-parent ont::not-accessible-val)
     (example "the routes are inaccessible by helicopter")
     (SEM (F::GRADABILITY +))
     (TEMPL central-adj-xp-templ (XP (% W::PP (W::PTYPE (? pt W::on W::for w::by)))))
     (meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090731 :wn ("inaccessible%3:00:00") :comments nil)
     )
    )
   )
))

