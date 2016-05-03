;;;;
;;;; W::DISTINCT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::DISTINCT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("distinct%5:00:00:different:00"))
     (EXAMPLE "They are distinct [from each other]")
     (LF-PARENT ONT::DIFFERENT)
     (SEM (F::GRADABILITY F::+))
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE W::FROM))))
     )
    )
   )
))

