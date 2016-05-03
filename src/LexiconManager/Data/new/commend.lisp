;;;;
;;;; W::commend
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::commend
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("commend%2:32:00" "commend%2:32:02"))
     (LF-PARENT ONT::praise)
     (TEMPL agent-addressee-templ) ; like thank
     )
     ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::praise)
     (example "he commended the proposal")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

