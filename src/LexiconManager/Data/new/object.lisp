;;;;
;;;; W::object
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::object
   (SENSES
    ((EXAMPLE "I found an interesting object")
     (LF-PARENT ONT::phys-object)
     (meta-data :origin boudreaux :entry-date 20031026 :wn ("object%1:03:00"))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
 (w::object
   (wordfeats (W::morph (:forms (-vb) :nom w::objection)))
	   (senses
	    (;;(LF-parent ont::contest)
	     (lf-parent ont::object) ;; 20120524 GUM change new parent
	     (Example "He objects to the findings")
	     (TEMPL AGENT-FORMAL-XP-TEMPL  (xp (% w::PP (w::ptype w::to))))
	     (meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090508 :comments caloy3) 
	     )	    
	    ))
))

