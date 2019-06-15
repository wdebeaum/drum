;;;;
;;;; W::CASH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CASH
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("cash%1:21:00"))
     (LF-PARENT ONT::MONEY)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
;   )
  (W::cash
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("get-13.5.1-1"))
     (LF-PARENT ONT::commerce-sell)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

