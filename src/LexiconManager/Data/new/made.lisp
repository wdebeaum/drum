;;;;
;;;; W::made
;;;;

(define-words :pos W::v
  :words (
	  (w::made
	   (wordfeats (W::VFORM W::PASSIVE) (W::AGR ?agr) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "The hat is made of paper")
	     (LF-PARENT ONT::comprise)
	     (TEMPL neutral-neutral1-xp-templ (xp (% w::pp (w::ptype (? p w::of)))))
	     )
	    )
	   ))
  )
