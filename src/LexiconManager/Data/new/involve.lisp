;;;;
;;;; W::involve
;;;;

(define-words :pos W::v 
 :words (
  (W::involve
   (wordfeats (W::morph (:forms (-vb) :nom w::involvement :nomsubjpreps (w::in))))
   (SENSES
    ((EXAMPLE "it involves a wireless card")
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     (LF-PARENT ONT::HAVE)
     (SEM (F::Aspect F::static) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )
    )
   )
))

;; extra form for passive
(define-words :pos W::v
  :words (
	  (W::involved
	   (wordfeats (W::VFORM W::PASSIVE) (W::AGR ?agr) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "I am involved in that")
	     (LF-PARENT ONT::HAVE)
	     (meta-data :origin calo :entry-date 20041122 :change-date 20090512 :comments caloy2)
	     (TEMPL neutral1-neutral-TEMPL (xp (% w::pp (w::ptype (? p w::in)))))
	     )
	    )
	   ))
  )
