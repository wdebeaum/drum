;;;;
;;;; W::SEA
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::SEA
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("sea%1:17:00"))
     (LF-PARENT ONT::geo-formation)
     )
    )
   )
))

(define-words :pos w::N 
 :words (
  ((w::sea w::lion)
  (senses((LF-parent ONT::nonhuman-animal) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::n
 :words (
  ((W::SEA W::BASS)
  (senses
	   ((LF-PARENT ONT::SALTWATER-fish)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::SEA W::CUCUMBER)
  (senses
	   ((LF-PARENT ONT::SALTWATER-fish)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n :templ count-pred-templ
 :tags (:base500)
 :words (
 (W::sea
   (SENSES
    ((meta-data :origin bolt :entry-date 20120516 :comments top500)
     (LF-PARENT ONT::geo-formation)
     )
    )
   )
))

