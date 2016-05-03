;;;;
;;;; w::south
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (w::south
   (SENSES
    ((meta-data :origin coordops :entry-date 20070516 :change-date nil :comments nil)
     (LF-PARENT ONT::cardinal-point)
     (example "turn right to south")
     (TEMPL other-reln-TEMPL)
     )
    )
   )
))

(define-words :pos w::N 
 :words (
  ((w::south w::american)
  (senses((LF-parent ONT::nationality-val) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::SOUTH
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("south%3:00:00"))
     (LF-PARENT ONT::SOUTH)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::SOUTH W::OF)
   (SENSES
    ((LF-PARENT ONT::south-reln)
     (LF-FORM W::SOUTH-of)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::SOUTH
   (SENSES
    ((LF-PARENT ONT::CARDINAL-DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

