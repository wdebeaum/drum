;;;;
;;;; W::curse
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::curse
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::insult)
     (example "he cursed him")
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::insult)
     (example "he cursed it" "that cursed pill")
     (TEMPL agent-theme-xp-templ) 
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::exclamation)
     (example "he cursed")
     (preference .98)
     (TEMPL agent-templ) 
     )
    )
   )
))

