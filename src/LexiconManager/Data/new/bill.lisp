;;;;
;;;; W::BILL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::BILL
     (SENSES
      ((meta-data :origin calo :entry-date 20040505 :change-date nil :wn ("bill%1:10:01") :comments calo-y1variants)
       (example "put it on my bill")
       (LF-PARENT ONT::account-payable)
       (templ other-reln-templ)
       )
      )
     )
))

(define-words :pos W::v 
 :words (
  (W::bill
   (SENSES
    ((meta-data :origin calo :entry-date 20040407 :change-date nil :comments y1-variations)
     (EXAMPLE "bill the trip to my account")
     (LF-PARENT ONT::commerce-collect)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-affected-SOURCE-TEMPL (xp (% W::pp (W::ptype W::to))))
     (TEMPL AGENT-AFFECTED-AFFECTEDR-XP-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20040407 :change-date nil :comments y1-variations)
     (EXAMPLE "bill my account (for the trip)")
     (LF-PARENT ONT::commerce-collect)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-SOURCE-TEMPL)
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::for)))))
     )
    )
   )
))

