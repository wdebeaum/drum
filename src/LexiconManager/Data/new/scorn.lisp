;;;;
;;;; W::scorn
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::scorn
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090507 :comments nil :vn ("judgement-33") :wn ("scorn%2:37:00"))
     (LF-PARENT ONT::manner-say)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090507 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::manner-say)
     (example "he scorned it")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

