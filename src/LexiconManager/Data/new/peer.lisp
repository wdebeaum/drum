;;;;
;;;; W::peer
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::peer
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments nil)
     (LF-PARENT ONT::professional)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::peer
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("peer-30.3") :wn ("peer%2:39:00"))
     (LF-PARENT ONT::scrutiny)
     (TEMPL agent-templ) ; like look
     )
    )
   )
))

