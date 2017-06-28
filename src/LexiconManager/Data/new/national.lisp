;;;;
;;;; w::national
;;;;

(define-words :pos w::N
 :words (
  (w::national
  (senses((LF-parent ONT::inhabitant)
            (templ count-pred-templ)
            (meta-data :origin adjective-reorganization :entry-date 20170427 :change-date nil)
	    (example "Canadian national")
            ))
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::national
   (SENSES
    ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
     (LF-PARENT ONT::national-val)
     (example "national government")
     )
    )
   )
))

