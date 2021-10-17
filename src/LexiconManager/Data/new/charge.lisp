;;;;
;;;; W::CHARGE
;;;;

(define-words :pos W::n 
 :words (
    ((W::CHARGE w::CARD)
     (SENSES
    ((meta-data :origin calo :entry-date 20040205 :change-date nil :wn ("charge_card%1:21:00") :comments y1v5)
     (LF-PARENT ONT::credit-card)
     (templ other-reln-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::charge
   (SENSES
    ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::for))))
     (example "there is an extra charge for gift wrapping")
     )
    
    ((lf-parent ont::physical-phenomenon)
     (example "the charge in the cylinder ignites")
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil))
    )
   )
))

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::charge
	   (senses
	    ((lf-parent ont::electric-measure-scale)
	     (syntax (w::mass w::mass))
	     (meta-data :origin bee :entry-date 20040407 :change-date nil :wn ("charge%1:19:00") :comments test-s)
	     )
	    ))
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::charge
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("charge%2:33:00" "charge%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20040407 :change-date nil :comments y1-variations)
     (EXAMPLE "charge it to my account")
     (LF-PARENT ONT::commerce-collect)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-AFFECTED-SOURCE-XP-TEMPL (xp (% W::pp (W::ptype (? pt W::to W::on)))))
     (TEMPL AGENT-AFFECTED-AFFECTEDR-XP-TEMPL (xp (% W::pp (W::ptype (? pt W::to W::on))))) ; copied from bill.lisp
     )
    ((meta-data :origin calo :entry-date 20040407 :change-date nil :comments y1-variations)
     (EXAMPLE "charge my account")
     (LF-PARENT ONT::commerce-collect)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-SOURCE-XP-TEMPL)
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::for))))) ; copied from bill.lisp
     )
    ((meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
     (EXAMPLE "charge the battery")
     (LF-PARENT ONT::fill-container)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

