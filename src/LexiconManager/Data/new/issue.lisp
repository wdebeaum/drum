;;;;
;;;; W::issue
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::issue
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :wn ("issue%1:09:01" "issue%1:09:00") :comments s11)
     (LF-PARENT ONT::situation)
     )
    )
   )
))

(define-words :pos W::V 
 :words (
  (W::issue
      ;(wordfeats (W::morph (:forms (-vb) :nom w::issuance)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("future_having-13.3") :wn ("issue%2:41:00"))
     (LF-PARENT ONT::supply)
     (example "issue him a visa")
     (TEMPL AGENT-AFFECTED-TEMPL) ; like grant,offer
     )
    )
   )
))

