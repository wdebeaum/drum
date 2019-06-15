;;;;
;;;; W::cull
;;;;

(define-words :pos W::v 
 :words (
  (W::cull
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090529 :comments nil :vn ("cheat-10.6"))
     (LF-PARENT ONT::discard)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090529 :comments nil :vn ("remove-10.1") :wn ("cull%2:35:00" "cull%2:40:00"))
     (LF-PARENT ONT::discard)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::from)))) ; like eliminate
     )
    )
   )
))

