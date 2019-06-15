;;;;
;;;; W::INTEREST
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::INTEREST
   (SENSES

     ((LF-PARENT ONT::value-COST)
     (TEMPL  reln-subcat-of-units-TEMPL)
     (example "mortgage interest of $1000 ")
     (meta-data :origin windenergy :entry-date 20080521 :change-date nil :wn ("tax%1:21:00") :comments nil)
      )
     ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::on))))
     (example "what is the interest on the loan")
      )
     )
   )
  )
 )

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::interest
    (wordfeats (W::morph (:forms (-vb) :nom w::interest)))
   (SENSES
    ((EXAMPLE "does that interest you")
     (LF-PARENT ONT::evoke-attention)
     (meta-data :origin calo :entry-date 20041122 :change-date 20090512 :comments caloy2)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

