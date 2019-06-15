;;;;
;;;; W::estimate
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::estimate
   (SENSES
    ((LF-PARENT ONT::information-function-object)
     (example "get an estimate of how much it will cost")
     (meta-data :origin calo :entry-date 20041130 :change-date nil :wn ("estimate%1:09:00") :comments caloy2 :wn-sense (4))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::estimate
   (SENSES
    ;;;; swier -- I think the ontology should be expanded to include the class of "estimateable" things.
    ;;;; numberical things can be estimated, you can estimate time, mass, number, etc. But you cannot
    ;;;; estimate oranges. And you cannot estimate a route.
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090522 :comments nil :vn ("price-54.4") :wn ("estimate%2:31:00"))
     (LF-PARENT ONT::becoming-aware-of-value)
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     (example "estimate the number of oranges")
     )
    )
   )
))

