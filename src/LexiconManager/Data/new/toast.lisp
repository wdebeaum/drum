;;;;
;;;; w::toast
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::toast
 (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("toast%2:34:00"))
     (LF-PARENT ONT::congratulate)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     (PREFERENCE 0.96)
     )
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cook-result)
   (example "toast the bread")
   (syntax (w::resultative +))
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )
  )
 )
))

