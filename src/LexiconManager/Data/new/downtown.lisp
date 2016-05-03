;;;;
;;;; W::DOWNTOWN
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DOWNTOWN
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("downtown%1:15:00"))
     (LF-PARENT ONT::region-for-activity)
     (SEM (F::spatial-abstraction (? sa F::spatial-point F::SPATIAL-REGION)))
     (TEMPL BARE-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   (W::downtown
   (SENSES
    ((LF-PARENT ONT::downtown)
     (TEMPL PRED-S-VP-TEMPL)
     (example "he went downtown")
     (meta-data :origin calo-ontology :entry-date 20060418 :change-date nil :comments nil)
     )
    )
   )
))

