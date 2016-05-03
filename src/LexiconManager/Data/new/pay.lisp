;;;;
;;;; W::pay
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::pay
   (wordfeats (W::morph (:forms (-vb) :past W::paid :ing w::paying)))
   (SENSES
    ((LF-PARENT ONT::commerce-pay)
     (TEMPL agent-cost-templ)
     (example "he paid 5 dollars")
     (meta-data :origin calo :entry-date 20040504 :change-date nil :comments calo-y1variants)
     )
    ((meta-data :origin calo :entry-date 20031219 :change-date nil :comments nil :vn ("pay-68"))
     (LF-PARENT ONT::commerce-pay)
     (TEMPL AGENT-COST-neutral-TEMPL (xp (% W::PP (W::ptype W::for))))
     (example "he paid 5 dollars for it")
     )
    ((LF-PARENT ONT::commerce-pay)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::atomic))
     (example "he paid for the truck")
     (meta-data :origin calo :entry-date 20040504 :change-date nil :comments calo-y1variants)
     (TEMPL agent-neutral-xp-TEMPL (xp (% W::PP (W::ptype W::for))))
     )
    ((LF-PARENT ONT::commerce-pay)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::atomic))
     (example "pay the money to me")
     (meta-data :origin calo :entry-date 20040504 :change-date nil :comments calo-y1variants)
     (TEMPL AGENT-cost-goal-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    ((LF-PARENT ONT::commerce-pay)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::atomic))
     (example "pay me the money")
     (meta-data :origin calo :entry-date 20040504 :change-date nil :comments calo-y1variants :vn ("pay-68"))
     (TEMPL agent-goal-affected-templ)
     )
    ;; need to subcat for this use because the definition of ont::instrument is currently limited to phys-obj
    ;; and credit card is classified as an account, which is an abstr-obj (e.g. can also be a grant)
    #||((meta-data :origin calo :entry-date 20040505 :change-date nil :comments y1-variations :vn ("pay-68"))
     (EXAMPLE "pay with a credit card")
     (LF-PARENT ONT::commerce-pay)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT--TEMPL (xp (% W::pp (W::ptype W::with))))
     )||#
        )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
	      :words (
		      ((W::pay W::attention)
		       (wordfeats (W::morph (:forms (-vb) :past W::paid)))
		       (SENSES
			((LF-PARENT ONT::scrutiny)
			 (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
			 (example "pay attention")
			 (TEMPL agent-templ)
			 )
			((LF-PARENT ONT::scrutiny)
			 (example "pay attention to the road")
			 (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
			 (TEMPL agent-neutral-xp-templ (xp (% W::pp (W::ptype W::to))))
			 )
			))
		      ))

