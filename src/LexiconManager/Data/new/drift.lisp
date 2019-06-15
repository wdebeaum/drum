;;;;
;;;; W::drift
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::drift
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("drift%2:38:02" "drift%2:38:04" "drift%2:38:06"))
     (LF-PARENT ONT::drift)
     (TEMPL affected-templ) ; like move,bounce
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("drift%2:35:03" "drift%2:38:02" "drift%2:38:04" "drift%2:38:05" "drift%2:38:06"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

