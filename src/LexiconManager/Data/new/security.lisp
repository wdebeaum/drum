;;;;
;;;; W::security
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::security
   (SENSES
    (;(LF-PARENT ONT::confidentiality-scale)
     (LF-PARENT ONT::safety-scale)
     ;(TEMPL MASS-PRED-TEMPL)
     (templ other-reln-templ)
     (EXAMPLE "iCal includes security enhancements")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("security%1:26:00") :comments nil)
     )
    )
   )
))

