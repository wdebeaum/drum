;;;;
;;;; W::dump
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::dump
   (SENSES
    ;;;; swier -- dump the oranges off the truck
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("funnel-9.3-2-1"))
     (LF-PARENT ONT::put)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     
     (TEMPL agent-affected-xp-templ)
     )
    
   )
   )))

