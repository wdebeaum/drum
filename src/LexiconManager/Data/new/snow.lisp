;;;;
;;;; W::snow
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::snow
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("snow%1:19:00"))
     (LF-PARENT ONT::precipitation)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
   (w::snow
   (senses
    ((LF-PARENT ont::precipitating)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL EXPLETIVE-TEMPL)
     (EXAMPLE "it started snowing")
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date nil :comments caloy3)
     )
    )
   )
))

