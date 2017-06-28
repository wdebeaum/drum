;;;;
;;;; w::british
;;;;

(define-words :pos w::n 
 :words (
  (w::british
  (senses((LF-parent ONT::nationality)
            (templ count-pred-templ)
            (meta-data :origin adjective-reorganization :entry-date 20170427 :change-date nil)
            ))
)
))

(define-words :pos w::adj 
 :words (
  (w::british
  (senses((LF-parent ONT::nationality-val) 
	    (templ central-adj-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))





