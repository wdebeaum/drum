;;;;
;;;; W::speakerphone
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::speakerphone
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :wn ("speakerphone%1:06:00") :comments plow-req)
     (example "I want to buy a speakerphone")
     )
    )
   )
))

