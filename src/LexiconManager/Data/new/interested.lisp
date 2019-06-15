;;;;
;;;; W::interested
;;;;

(define-words :pos W::v
  :tags (:base500)
  :words (
	  (W::interested
	   (wordfeats (W::VFORM W::PASSIVE) (W::AGR ?agr) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "I am interested in that")
	     (LF-PARENT ONT::evoke-attention)
	     (meta-data :origin calo :entry-date 20041122 :change-date 20090512 :comments caloy2)
	     (TEMPL AGENT-NEUTRAL-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? p w::in)))))
	     )
	    )
	   ))
  )

