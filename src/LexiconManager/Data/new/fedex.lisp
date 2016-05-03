;;;;
;;;; W::FedEx
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::FedEx
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::send)
     (TEMPL agent-affected-recipient-alternation-templ) ; like mail,send,forward,transmit
     )
    )
   )
))

