;;;;
;;;; w::toast
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::toast
 (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("toast%2:34:00"))
     (LF-PARENT ONT::congratulate)
     (TEMPL agent-addressee-templ) ; like thank
     (PREFERENCE 0.96)
     )
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "toast the bread")
   (syntax (w::resultative +))
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-XP-TEMPL)
   )
  )
 )
))

