;;;;
;;;; W::responsible
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::responsible
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    #||((meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :wn ("responsible%3:00:00") :comments caloy3)     
     (EXAMPLE "responsible person")
     (lf-parent ont::responsible-val)
     (TEMPL central-adj-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )||#
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :wn ("responsible%3:00:00") :comments caloy3)     
     (EXAMPLE "responsible person")
     (lf-parent ont::responsible-val)
     (TEMPL adj-co-theme-templ (xp (% W::PP (w::ptype w::for))))
     )
    )
   )
))

