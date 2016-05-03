;;;;
;;;; W::delegate
;;;;

(define-words :pos W::v 
 :words (
  (W::delegate
   (SENSES
    ((EXAMPLE "delegate the task (to him)")
     (LF-PARENT ONT::ASSIGN)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;; this verb doesn't participate in the alternation
     (TEMPL agent-affected-goal-optional-templ) 
     (meta-data :origin boudreaux :entry-date 20031024 :change-date 20060120 :comments "EVC 2.1")
     )
    )
   )
))

