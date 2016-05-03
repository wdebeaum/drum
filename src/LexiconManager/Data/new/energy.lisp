;;;;
;;;; W::ENERGY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ENERGY
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("energy%1:19:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::substance)
     (SEM (f::form f::wave))
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::ENERGY W::DRINK)
  (senses
	   ((LF-PARENT ONT::SODA)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

