;;;;
;;;; W::microwave
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::microwave
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("microwave%2:30:00"))
     (LF-PARENT ONT::cook-in-microwave)
     (EXAMPLE "He microwaved the salmon")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

(define-words :pos W::n :templ count-pred-templ
 :words (
  (W::microwave
   (SENSES
    ((meta-data :origin caet :entry-date 20130523 :change-date nil :comments icmi)
     (LF-PARENT ONT::microwave)

     )
    )
   )
))
