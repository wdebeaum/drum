;;;;
;;;; W::jar
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::jar
   (SENSES
    ((meta-data :origin trips :entry-date 20081212 :change-date nil :comments nil :wn ("bottle%1:06:00" "bottle%1:06:01"))
     (LF-PARENT ONT::small-container)
     (TEMPL pred-subcat-contents-templ)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::jar
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("jar%2:37:00"))
     (LF-PARENT ONT::evoke-distress)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

