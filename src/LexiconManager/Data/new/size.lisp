;;;;
;;;; W::SIZE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SIZE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("size%1:07:00"))
     (LF-PARENT ONT::size-scale)
     (TEMPL OTHER-RELN-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("size%1:07:00"))
     (LF-PARENT ONT::size-scale)
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
(w::size
 (senses
  ((meta-data :origin calo :entry-date 20050425 :change-date 20090507 :comments projector-purchasing :wn ("size%2:30:00"))
   (LF-PARENT ONT::change-magnitude)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-XP-TEMPL)
   )
  )
 )
))

