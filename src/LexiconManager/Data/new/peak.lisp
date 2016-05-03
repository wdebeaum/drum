;;;;
;;;; W::peak
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::peak
   (SENSES
    ((meta-data :origin caloy3 :entry-date 20070330 :change-date nil :wn ("researcher%1:18:00") :comments caloy3-test-data)
     (LF-PARENT ONT::geo-formation)
     )
    )
   )
))

(define-words :pos W::n :templ count-pred-templ
 :words (
	 ((W::peak w::flow w::meter)
	  (SENSES
	   ((LF-PARENT ONT::device)
	    (meta-data :origin asma :entry-date 20110829 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

