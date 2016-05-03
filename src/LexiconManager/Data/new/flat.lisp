;;;;
;;;; W::FLAT
;;;;

(define-words :pos W::n
 :words (
  ((W::FLAT W::HALF)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::FLAT
   (wordfeats (w::comparative +) (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040407 :change-date 20090731 :wn ("flat%5:00:00:planar:00") :comments y1v4)
     (EXAMPLE "a flat screen" "a flat stomach")
     (LF-PARENT ONT::SLIGHT)
     )
    )
   )
))

