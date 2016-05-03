;;;;
;;;; W::sleet
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sleet
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("sleet%1:19:00"))
     (LF-PARENT ONT::precipitation)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::sleet
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("sleet%2:43:00"))
     (LF-PARENT ONT::precipitating)
     (TEMPL expletive-templ) ; like rain
     )
    )
   )
))

