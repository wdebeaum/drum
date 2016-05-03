;;;;
;;;; w::polar
;;;;

(define-words :pos w::N 
 :words (
  ((w::polar w::bear)
  (senses((LF-parent ONT::nonhuman-animal) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::polar
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :wn ("polar%5:00:00:charged:00") :comments caloy3)
     (LF-PARENT ONT::polarity-VAL)
     )
    )
   )
))

