;;;;
;;;; W::reassign
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::reassign
   (SENSES
    ((LF-PARENT ONT::assign)
     (example "reassign the task (to group a)")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-recipient-alternation-templ) ; like grant,offer
     (meta-data :origin lou :entry-date 20040311 :change-date 20060120 :comments lou-sent-entry)
     )
    )
   )
))

