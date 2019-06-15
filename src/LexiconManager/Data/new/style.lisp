;;;;
;;;; W::style
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::style
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("style%1:09:01"))
     (LF-PARENT ONT::version)
    (TEMPL GEN-PART-OF-RELN-TEMPL)
    )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTEDR-XP-TEMPL
 :words (
  (W::style
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("create-26.4") :wn ("style%2:36:01"))
     (LF-PARENT ONT::create)
 ; like produce
     )
    ))
   ))

