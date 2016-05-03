;;;;
;;;; w::north
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (w::north
   (SENSES
    ((meta-data :origin coordops :entry-date 20070516 :change-date nil :comments nil)
     (LF-PARENT ONT::cardinal-point)
     (example "turn right to north")
     (TEMPL other-reln-TEMPL)
     )
    )
   )
))

(define-words :pos w::N 
 :words (
  ((w::north w::american)
  (senses((LF-parent ONT::nationality-val) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::NORTH
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("north%3:00:00"))
     (LF-PARENT ONT::NORTH)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::NORTH
   (SENSES
    ((LF-PARENT ONT::CARDINAL-DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::NORTH W::OF)
   (SENSES
    ((LF-PARENT ONT::north-reln)
     (LF-FORM W::NORTH-of)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

