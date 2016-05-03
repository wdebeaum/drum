;;;;
;;;; W::KB
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::KB ;; alternate plural
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20070813 :comments nil :wn ("mb%1:23:00"))
     (LF-PARENT ONT::memory-unit)
     (LF-FORM W::kilobyte)
     (TEMPL SUBSTANCE-UNIT-lf-of-3s-plur-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::kb
   (SENSES
    ((LF-PARENT ONT::memory-unit)
     (TEMPL SUBSTANCE-UNIT-lf-of-3s-TEMPL)
     (EXAMPLE "26 messages (343 KB)")
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :wn ("kb%1:23:00") :comments nil)
     )
    )
   )
))

