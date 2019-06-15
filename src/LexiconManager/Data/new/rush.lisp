;;;;
;;;; W::rush
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::rush
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("rush%2:38:00" "rush%2:38:10"))
     (LF-PARENT ONT::move-rapidly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    ((lf-parent ont::increase-speed)
     (example "rush the process")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
    )
    )
   )
))

