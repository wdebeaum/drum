;;;;
;;;; W::CHILL
;;;;

(define-words :pos W::n
 :words (
  (W::CHILL
  (senses
   ((LF-PARENT ONT::physical-symptom)
    (TEMPL count-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
((W::chill w::out)
   (SENSES
    ((LF-PARENT ont::wait)
     (example "chill out")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
(w::chill
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::cool)
   (example "chill the juice" "the wind chilled him")
   (syntax (w::resultative +))
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
    )
  )
 )
))

