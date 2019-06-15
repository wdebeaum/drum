;;;;
;;;; W::unite
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::unite
   (SENSES
    ((EXAMPLE "unite this cell with the next cell")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060605 :change-date nil :comments nil :vn ("amalgamate-22.2-2-1") :wn ("unite%2:30:00" "?unite%2:42:01" "unite%2:42:02" "unite%2:30:02"))
     )
    ((EXAMPLE "unite them together")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-affected-PLURAL-TEMPL)
     )
    )
   )
  )
 )


