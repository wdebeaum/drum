;;;;
;;;; W::upload
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::upload
   (SENSES
    ((EXAMPLE "upload an image to hab")
     (meta-data :origin boudreaux :entry-date 20050824 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::submit)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

