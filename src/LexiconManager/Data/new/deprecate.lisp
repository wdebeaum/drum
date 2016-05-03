;;;;
;;;; W::deprecate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::deprecate
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090507 :comments nil :vn ("judgement-33") :wn ("deprecate%2:32:00" "deprecate%2:32:01"))
     (LF-PARENT ONT::criticize)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090507 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::criticize)
     (example "he deprecated it")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

