;;;;
;;;; W::scroll
;;;;

(define-words :pos W::v 
 :words (
  (W::scroll
   (SENSES
    ((example "scroll up/down the page/display/screen")
     (LF-PARENT ONT::scroll)
     (TEMPL AGENT-TEMPL)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin plow :entry-date 20050929 :change-date nil :comments naive-subjects)
     )
    )
   )
))

