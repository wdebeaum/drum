;;;;
;;;; W::SOFT
;;;;

(define-words :pos W::n
 :words (
  ((W::SOFT W::DRINK)
  (senses
	   ((LF-PARENT ONT::SODA)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::SOFT W::RED W::WINTER W::WHEAT)
  (senses
	   ((LF-PARENT ONT::GRAINS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::SOFT
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("soft%3:00:01") :comments html-purchasing-corpus)
     (LF-PARENT ONT::soft-val)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("soft%3:00:04") :comments html-purchasing-corpus)
     (LF-PARENT ONT::QUIET)
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

