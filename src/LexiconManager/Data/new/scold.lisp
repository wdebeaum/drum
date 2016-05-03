;;;;
;;;; W::scold
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::scold
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090507 :comments nil :vn ("judgement-33") :wn ("scold%2:32:00"))
     (LF-PARENT ont::reprimand)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090507 :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::reprimand)
     (example "he scolded it")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

