;;;;
;;;; W::phone
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::phone
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo :entry-date 20041206 :change-date nil :wn ("phone%1:06:00") :comments caloy2)
     (example "this company offers phone support")
;     (preference .98) ;; prefer compounds for plot
     )
    )
   )
))

(define-words :pos W::V 
 :words (
  (W::phone
   (SENSES
    ((EXAMPLE "phone the doctor")
     (LF-PARENT ONT::establish-communication)
     (TEMPL AGENT-ADDRESSEE-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060214 :change-date 20090508 :comments nil)  
     (preference .97) ;; prefer noun sense
     )
    ))
   )
)

