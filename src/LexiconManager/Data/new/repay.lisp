;;;;
;;;; W::repay
;;;;

(define-words :pos W::v 
 :words (
  (W::repay
   (wordfeats (W::morph (:forms (-vb) :past W::repaid :ing W::repaying :nom w::repayment)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("pay-68"))
     (LF-PARENT ONT::commerce-pay)
     (TEMPL agent-goal-affected-templ (xp (% w::pp (w::ptype w::for)))) ; like pay
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("pay-68"))
     (LF-PARENT ONT::commerce-pay)
     (TEMPL AGENT-cost-goal-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    #||((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("pay-68"))
     (LF-PARENT ONT::commerce-pay)
     (TEMPL agent-source-templ (xp (% w::pp (w::ptype w::with)))) ; like pay
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("judgement-33") :wn ("repay%2:40:02"))
     (LF-PARENT ONT::compensate)
     (example "how can I repay you")
     (TEMPL agent-addressee-templ) ; like thank
     )
    )
   )
))

