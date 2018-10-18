;;;;
;;;; W::LUNCH
;;;;

(define-words :pos W::n
 :words (
  (W::LUNCH
  (senses;;;;; meals may or may not have articles
   ((LF-PARENT ONT::meal-event)    
    (TEMPL BARE-PRED-TEMPL)
    )
   ((LF-PARENT ONT::food)
    (TEMPL BARE-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::lunch
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2"))
     (LF-PARENT ONT::consume)
     (templ agent-affected-optional-templ  (xp (% W::pp (W::ptype W::on)))) ; like dine
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((w::lunch w::time)
  (senses;;;;; night is separate because we can have it with or without articles
   (;(LF-PARENT ONT::time-interval)
    (LF-PARENT ONT::recurring-time-of-day)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

