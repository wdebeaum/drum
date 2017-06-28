;;;;
;;;; W::perpendicular
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::perpendicular
   (SENSES
    ((LF-PARENT ONT::VERTICAL)
     (meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090731 :wn ("perpendicular%3:00:04") :comments nil)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::to))))
     (EXAMPLE "the axes are perpendicular to each other")
     )
    ((LF-PARENT ONT::geometric-relationship-val)
     (meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090731 :wn ("perpendicular%3:00:00") :comments nil)
     (EXAMPLE "measure the perpendicular height")
     )
    )
   )
))

