;;;;
;;;; w::boil
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::boil
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
 ;  (LF-PARENT ONT::cooking)
   (lf-parent ont::boil)
   (syntax (w::resultative +))
   (example "boil an egg")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-XP-TEMPL)
   )
   ((meta-data :origin make-tea :entry-date 20100930 :change-date nil :comments nil)
 ;  (LF-PARENT ONT::cooking)
    (lf-parent ont::boil)
   (example "the water boiled")
   (templ affected-templ)
   )
  )
 )
))

