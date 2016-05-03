;;;;
;;;; W::INTEREST
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::INTEREST
   (SENSES
;    ((meta-data :origin calo :entry-date 20031229 :change-date nil :wn ("interest%1:09:00") :comments html-purchasing-corpus)
;     (LF-PARENT ONT::mental-object)
;     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt W::in)))))
;     )
     ((LF-PARENT ONT::value-COST)
     (TEMPL OTHER-RELN-TEMPL)
     (example "mortgage interest")
     (meta-data :origin windenergy :entry-date 20080521 :change-date nil :wn ("tax%1:21:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::interest
    (wordfeats (W::morph (:forms (-vb) :nom w::interest)))
   (SENSES
    ((EXAMPLE "does that interest you")
     (LF-PARENT ONT::evoke-attention)
     (meta-data :origin calo :entry-date 20041122 :change-date 20090512 :comments caloy2)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

