;;;;
;;;; W::PRICE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PRICE
   (SENSES
    ((LF-PARENT ONT::price)
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script )
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::price
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090522 :comments nil :vn ("price-54.4") :wn ("price%2:31:00" "price%2:40:00"))
     (LF-PARENT ONT::scrutiny)
     (templ agent-neutral-xp-templ)
     )
    )
   )
))

