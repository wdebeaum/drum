;;;;
;;;; W::articulate
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::articulate
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("articulate%2:32:01" "articulate%2:32:02"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::say)
     (TEMPL agent-theme-to-addressee-optional-templ)  ; like say but needs different template b.c. doesn't participate in alternation

     )
    )
   )
))

