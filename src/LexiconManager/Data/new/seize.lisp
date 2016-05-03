;;;;
;;;; W::seize
;;;;

(define-words :pos W::v 
 :words (
  (W::seize
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090430 :comments nil :vn ("obtain-13.5.2") :wn ("seize%2:35:00" "seize%2:35:01" "seize%2:40:01" "seize%2:40:02"))
     (LF-PARENT ONT::appropriate)
     (example "the theif seized the jewels from the safe")
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like recover
     )
    ((meta-data :origin cardiac :entry-date 20081215 :change-date 20090512 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::evoke-attention)
     (TEMPL agent-affected-xp-templ)
     (example "pain seized his body")
     )
    ((meta-data :origin cardiac :entry-date 20081215 :change-date 20090512 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::cause-body-effect)
     (TEMPL affected-TEMPL)
     (example "his body seized (with pain/convulsions)")
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   ((W::seize w::up)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20081215 :change-date 20090511 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::cause-body-effect)
     (TEMPL affected-TEMPL)
     (example "his body seized up (with pain/convulsions)")
     )
    )
   )
))

