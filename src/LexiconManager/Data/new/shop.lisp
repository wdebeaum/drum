;;;;
;;;; W::shop
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::shop
   (SENSES
    ((LF-PARENT ONT::commercial-facility)
     (meta-data :origin caloy2 :entry-date 20050404 :change-date nil :wn ("shop%1:06:00") :comments projector-purchasing)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::shop
   (SENSES
    #|
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("search-35.2") :wn ("?shop%2:41:00"))
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL AGENT-FORMAL-XP-TEMPL) ; like check,search
     )
    |#

    ((LF-PARENT ONT::commercial-activity)
     (example "they shop a lot")
     (TEMPL agent-templ)
     )

    )
   )
))

