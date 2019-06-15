;;;;
;;;; W::exclaim
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::exclaim
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("say-37.7"))
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::MANNER-SAY)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-that)))) ; like disclose
     (PREFERENCE 0.96)
     )
    )
   )
))

