;;;;
;;;; W::available
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::available
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("available%5:00:00:disposable:00"))
     (EXAMPLE "that's available to go")
     (LF-PARENT ONT::AVAILABLE)
     (SEM (F::GRADABILITY F::+))
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-to))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("available%5:00:00:disposable:00"))
     (EXAMPLE "that's available [for use]")
     (LF-PARENT ONT::AVAILABLE)
     (SEM (F::GRADABILITY F::+))
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::pp (W::ptype (? pt W::for w::to)))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("available%5:00:00:disposable:00"))
     (example "the trucks available are one and three")
     (LF-PARENT ONT::AVAILABLE)
     (SEM (F::GRADABILITY F::+))
     (TEMPL postpositive-adj-optional-xp-templ)
     )
     ((meta-data :origin windmills :entry-date 20080606 :change-date 20090731 :comments nil :wn ("available%5:00:00:disposable:00"))
     (example "it is available in 4 MW capacity")
     (LF-PARENT ONT::AVAILABLE)
     (SEM (F::GRADABILITY F::+))
     (TEMPL adj-subcat-property-templ)
     )
    )
   )
))

