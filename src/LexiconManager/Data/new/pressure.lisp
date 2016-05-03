;;;;
;;;; W::pressure
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pressure
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (lf-parent ont::phys-measure-domain) 
     (templ other-reln-templ)
     )
    ((lf-parent ont::physical-phenomenon)
     (example "the pressure in the cylinder")
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil))
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::pressure
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He pressured him [to run for office]")  
     )
    )
   )
))

