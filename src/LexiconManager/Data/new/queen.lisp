
(define-words :pos W::n
 :words (
  ((W::QUEEN W::CRAB)
  (senses
	   ((LF-PARENT ONT::CRUSTACEANS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::queen
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (example "a room with a queen bed")
     (lf-parent ont::predefined-size-val)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  (w::queen
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

