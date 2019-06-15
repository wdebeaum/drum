;;;;
;;;; w::cow
;;;;

(define-words :pos w::N 
 :tags (:base500)
 :words (
  (w::cow
  (senses((LF-parent ONT::nonhuman-animal) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::cow
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("cow%2:37:00"))
     (LF-PARENT ONT::evoke-fear)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    )
   )
))

