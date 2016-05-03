;;;;
;;;; W::EQUIDISTANT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::EQUIDISTANT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("equidistant%5:00:00:equal:00"))
     (EXAMPLE "They are equidistant [between the lines]")
     (LF-PARENT ONT::DISTANCE-VAL)
     (SEM (F::GRADABILITY F::-))
     (TEMPL ADJ-CO-THEME-TEMPL (xp (% W::pp (W::ptype W::between))))
     )
    )
   )
))

