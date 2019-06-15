;;;;
;;;; W::blab
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::blab
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("blab%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::manner-say)
     (TEMPL AGENT-AFFECTED-FORMAL-2-XP-OPTIONAL-TEMPL (xp (% W::PP (w::ptype (? ptp w::to)))))
     )
    )
   )
))

