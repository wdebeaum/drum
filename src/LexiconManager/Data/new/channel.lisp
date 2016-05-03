;;;;
;;;; W::channel
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::channel
   (SENSES
    ;; a communication path -- should we create a specialized lf for that?
    ((LF-PARENT ONT::communication-channel)
     (example "the weather channel")
     (meta-data :origin calo-ontology :entry-date 20060712 :change-date nil :comments caloy3)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::CHANNEL W::CATFISH)
  (senses
	   ((LF-PARENT ONT::FRESHWATER-fish)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

