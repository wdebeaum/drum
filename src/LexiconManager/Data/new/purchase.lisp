;;;;
;;;; W::purchase
;;;;

(define-words :pos W::v 
 :words (
  (W::purchase
   (wordfeats (W::morph (:forms (-vb) :nom w::purchase)))
   (SENSES
    ((EXAMPLE "Purchase a computer")
      (meta-data :origin calo :entry-date 20040401 :change-date nil :comments y1-dialog-variations)
     (LF-PARENT ONT::commerce-buy)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
  #||  ((meta-data :origin calo :entry-date 20040505 :change-date nil :comments y1-variations)
     (EXAMPLE "purchase it with a credit card")
     (LF-PARENT ONT::commerce-pay)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-SOURCE-optional-TEMPL (xp (% W::pp (W::ptype (? pt W::with w::on)))))
     )||#
    )
   )
))

