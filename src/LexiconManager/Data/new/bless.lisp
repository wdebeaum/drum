;;;;
;;;; W::bless
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::bless
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("bless%2:32:00" "bless%2:40:00"))
     (LF-PARENT ONT::praise)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::praise)
     (example "he blessed it")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

