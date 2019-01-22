;;;;
;;;; W::egress
;;;;

(define-words :pos W::v 
 :words (
  (W::egress
   (SENSES
    ((EXAMPLE "They egressed the area by heading southwest.")
     (LF-PARENT ONT::depart)
;     (LF-PARENT ONT::cause-out-of)
     (meta-data :origin boudreaux :entry-date 20060414 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-TEMPL)     
     )    
    )
)))

