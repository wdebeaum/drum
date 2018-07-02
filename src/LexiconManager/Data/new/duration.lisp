;;;;
;;;; W::DURATION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DURATION
   (SENSES
    ((example "the duration of the project")
     (LF-PARENT ONT::DURATION-SCALE)
     (TEMPL OTHER-RELN-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("duration%1:28:02"))
     (LF-PARENT ONT::duration-SCALE)
     (example "a duration of five hours")
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))

