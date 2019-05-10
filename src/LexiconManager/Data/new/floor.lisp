;;;;
;;;; w::floor
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::floor
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments Office)
     (LF-PARENT ONT::structure-internal-component)
     (templ gen-part-of-reln-templ)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::floor
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("floor%2:37:00"))
     (LF-PARENT ONT::evoke-surprise)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

