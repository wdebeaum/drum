;;;;
;;;; W::worry
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::worry
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("worry%2:37:01" "worry%2:42:00"))
     (LF-PARENT ONT::evoke-distress)
     (TEMPL agent-affected-xp-templ) 
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::worrying)
     (TEMPL agent-templ) 
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090605 :comments nil :vn ("marvel-31.3-2"))
     (EXAMPLE "He worries about it ")
     (LF-PARENT ONT::worrying)
     (TEMPL agent-Theme-XP-TEMPL (xp (% W::PP (W::ptype (? p W::about)))))
     )
    )
   )
))

