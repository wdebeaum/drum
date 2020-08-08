;;;;
;;;; W::UNAVAILABLE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::UNAVAILABLE
   (wordfeats (W::comp-op W::LESS))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("unavailable%3:00:00"))
     (EXAMPLE "that's unavailable [to him]")
     (lf-parent ont::not-available-val)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::PTYPE W::to))))
     )
    ((meta-data :origin windmills :entry-date 20080606 :change-date nil :comments nil :wn ("available%5:00:00:disposable:00"))
     (example "it is unavailable in 4 MW capacity")
     (lf-parent ont::not-available-val)
     (SEM (F::GRADABILITY -))
     (TEMPL adj-subcat-property-templ)
     )
    )
   )
))

