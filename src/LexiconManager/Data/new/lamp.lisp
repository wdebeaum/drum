;;;;
;;;; W::lamp
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::lamp
    (SENSES
     ((meta-data :origin calo :entry-date 20050425 :change-date nil :wn ("lamp%1:06:00") :comment projector-purchasing)
      (LF-PARENT ONT::device-component)
      (example "this model has the second longest lasting lamp")
       (templ part-of-reln-templ)
      )
     ((meta-data :origin calo-ontology :entry-date 20060123 :change-date nil :wn ("lamp%1:06:01") :comment caloy3)
      (LF-PARENT ont::furnishings)
      (example "turn on the lamp")
      )
     )
    )
))

