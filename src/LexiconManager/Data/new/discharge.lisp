;;;;
;;;; W::discharge
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::discharge
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090528 :comments nil :vn ("remove-10.1") :wn ("discharge%2:29:00" "discharge%2:35:00" "discharge%2:35:06"))
     (LF-PARENT ONT::socially-remove)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
     (PREFERENCE 0.96)
     (example "they discharged him from the military")
     )
        
     (
      (lf-parent ont::emit-giveoff-discharge) ;; 20120524 GUM change new parent
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the spark plug discharges a spark")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

