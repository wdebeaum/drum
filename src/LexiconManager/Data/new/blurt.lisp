;;;;
;;;; W::blurt
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::blurt
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("blurt%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::manner-say)
     (TEMPL AGENT-FORMAL-AGENT1-OPTIONAL-TEMPL)  ; like say but needs different template b.c. doesn't participate in alternation
     ;(TEMPL agent-affected-iobj-theme-templ) ; like say
     )
    )
   )
))

