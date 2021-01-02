;;;;
;;;; W::REST
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::REST
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("rest%1:24:00"))
     (LF-PARENT ONT::remaining-PART)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("rest%1:24:00"))
     (LF-PARENT ONT::remaining-PART)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::rest
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put_spatial-9.2-1"))
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL NEUTRAL-TEMPL) ; like hang,stand,sit
     )

    ((meta-data :wn ("rest%2:41:00"))
     (LF-PARENT ONT::be-inactive)
     (TEMPL AGENT-TEMPL)
     )

    )
   )
))

