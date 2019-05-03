;;;;
;;;; W::sheet
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sheet
   (SENSES
    ((LF-PARENT ONT::info-medium)
     (EXAMPLE "specify attributes using a style sheet")
     (meta-data :origin task-learning :entry-date 20050816 :change-date nil :comments nil)
     )
    ((lf-parent ont::sheet)
     (example "sheet of paper/ice") 
     (meta-data :origin plow :entry-date 20060802 :change-date nil :wn ("sheet%1:06:03") :comments weather)
     )
     ((lf-parent ont::bedding)
     (example "put the sheet on the bed") 
     (templ count-pred-templ)
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     )
    )
   )
))

