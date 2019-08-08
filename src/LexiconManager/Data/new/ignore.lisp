;;;;
;;;; W::ignore
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::ignore
     (SENSES

;     ((LF-PARENT ONT::FORGET)
;      (meta-data :origin plow :entry-date 20050401 :change-date nil :comments nil)
;      (example "ignore that option")
;      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
;      (TEMPL AGENT-FORMAL-XP-TEMPL)
;      )
     
     ((LF-PARENT ONT::IGNORE)
      (example "ignore that option" "she ignored him at the meeting" "she ignored his advances")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL agent-affected-xp-np-templ)
     ) 
     
     )
     )
))

