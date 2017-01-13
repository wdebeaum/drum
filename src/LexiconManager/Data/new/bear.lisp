;;;;
;;;; w::bear
;;;;

(define-words :pos w::N 
 :words (
  (w::bear
  (senses((LF-parent ONT::nonhuman-animal) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
	 (W::bear
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060505 :change-date 20090511 :comments nil :vn ("admire-31.2"))
	    (EXAMPLE "I can't bear it")
	    (LF-PARENT ONT::enduring)
	    (TEMPL experiencer-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss,tolerate
	    )
	   )
	  )
))

(define-words :pos W::v :templ agent-affected-create-templ
 :words (
	 (W::bear
	  (wordfeats (W::morph (:forms (-vb) :past w::bore :pastpart w::born :nom w::birth)))
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060505 :change-date 20090511 :comments nil :vn ("admire-31.2"))
	    (EXAMPLE "I was born.")
	    (LF-PARENT ONT::be-born)
	    )
	   ))
	 
	 ))
