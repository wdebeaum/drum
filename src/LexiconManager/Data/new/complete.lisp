;;;;
;;;; W::COMPLETE
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::COMPLETE
   (SENSES
    ((LF-PARENT ONT::COMPLETE)
     (example "complete the plan")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::COMPLETE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("complete%3:00:00"))
     (LF-PARENT ONT::whole-complete) ;; like total
     )
    )
   )
))

