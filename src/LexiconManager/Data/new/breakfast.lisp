;;;;
;;;; W::BREAKFAST
;;;;

(define-words :pos W::n
 :words (
  ((W::BREAKFAST W::STRIP)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  (W::BREAKFAST
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
  (W::breakfast
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2"))
     (LF-PARENT ONT::consume)
     (preference .95)
     (templ agent-affected-optional-templ  (xp (% W::pp (W::ptype W::on)))) ; like dine
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((w::breakfast w::time)
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::recurring-time-of-day)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

