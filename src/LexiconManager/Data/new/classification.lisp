;;;;
;;;; W::classification
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::classification
   (SENSES
    ((EXAMPLE "document classification")
     (meta-data :origin integrated-learning :entry-date 20050816 :change-date nil :wn ("classification%1:04:00" "classification%1:14:00") :comments lf-restructure)
     (LF-PARENT ONT::grouping) ;; and/or ont::analytic-event?
     (TEMPL other-reln-templ)
     )
    )
   )
))

