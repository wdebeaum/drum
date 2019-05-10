;;;;
;;;; W::absorb
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::absorb
   (wordfeats (W::morph (:forms (-vb) :nom W::absorption)))
   (SENSES
    ((LF-PARENT ONT::take-in)
     (TEMPL agent-affected-xp-templ)
     )
    
    ((LF-PARENT ONT::come-to-understand)
     (meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("understand%2:31:00" "understand%2:31:01"))
     (example "I absorbed the news")
     ;;(SEM (F::Aspect F::Stage-level))
     (TEMPL agent-neutral-xp-templ)
     )
    ))))

