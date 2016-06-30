;;;;
;;;; W::HALF
;;;;

(define-words :pos W::n
 :words (
  ((W::HALF W::AND W::HALF)
  (senses
	   ((LF-PARENT ONT::MILK)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::HALF
   (wordfeats (W::status W::indefinite-plural) (W::negatable +) (W::NOsimple +) (W::NPmod +))
   (SENSES
    ((LF ONT::HALF)
     (non-hierarchy-lf t)(TEMPL quan-count-mass-TEMPL)
     (SYNTAX (W::agr (? agr W::3s W::3p)))
     )
    )
   )
))

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :tags (:base500)
 :words (
  (W::HALF
   (SENSES
    ((LF (W::NTH 2))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype w::fraction))
     )
    )
   )
))

