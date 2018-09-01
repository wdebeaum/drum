;;;;
;;;; W::graduate
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::graduate
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060712 :change-date nil :wn ("graduate%1:18:00") :comments caloy3)
     (LF-PARENT ONT::scholar)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
 (W::graduate
   (SENSES
    ((example "my son graduated from college")
     (sem (f::aspect f::dynamic))
     (meta-data :origin chf :entry-date 20070827 :change-date nil :comments chf-dialogues)
     (LF-PARENT ONT::advancing-status)
     (templ agent-source-optional-templ)
     )
    )
   )
))

