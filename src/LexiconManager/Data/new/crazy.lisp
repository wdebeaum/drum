;;;;
;;;; W::crazy
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::crazy
    (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
;    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090731 :wn ("crazy%5:00:00:insane:00" "crazy%5:00:00:impractical:00") :comments caloy3)
;     (EXAMPLE "he is a crazy person")
;     (LF-PARENT ONT::insane)
;     (TEMPL central-adj-experiencer-TEMPL)
;     (SEM (F::GRADABILITY +))
;     )
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090731 :wn ("reasonable%5:00:00:rational:00" "reasonable%3:00:00" "reasonable%5:00:00:moderate:00") :comments caloy3)
     (EXAMPLE "crazy idea")
     (LF-PARENT ONT::insane)
     (templ central-adj-templ)
;     (TEMPL central-adj-content-TEMPL)
     (SEM (F::GRADABILITY +))
     )
    )   
   )
))

