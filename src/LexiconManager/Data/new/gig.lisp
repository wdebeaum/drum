;;;;
;;;; W::gig
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::gig
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::memory-unit)
     (LF-FORM W::gigabyte)
     (TEMPL SUBSTANCE-UNIT-lf-of-3s-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::GIG ;; alternate plural
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20070813 :comments nil :wn ("mb%1:23:00"))
     (LF-PARENT ONT::memory-unit)
     (LF-FORM W::gigabyte)
     (example " 5 gig of ram")
     (TEMPL SUBSTANCE-UNIT-lf-of-3s-plur-TEMPL)
     )
    )
   )
))

