;;;;
;;;; W::entrance
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::entrance
   (SENSES
    ((meta-data :origin trips :entry-date 20091118 :change-date nil :comments nil :wn ("entrance%1:06:00"))
     (LF-PARENT ONT::exit)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::entrance
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("entrance%2:37:00"))
     (LF-PARENT ONT::evoke-attraction)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

