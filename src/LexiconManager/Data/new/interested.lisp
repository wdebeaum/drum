;;;;
;;;; W::interested
;;;;

;;  we have this to account for the irrgular passive -- using IN

(define-words :pos W::v
  :tags (:base500)
  :words (
	  (W::interested
	   (wordfeats (W::VFORM W::PASSIVE) (W::AGR ?agr) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "I am interested in that")
	     (LF-PARENT ONT::evoke-attention)
	     (meta-data :origin calo :entry-date 20041122 :change-date 20090512 :comments caloy2)
	     (TEMPL AFFECTED-AGENT-XP-PP-TEMPL (xp (% w::pp (w::ptype (? p w::in)))))
	     )
	    )
	   ))
  )

