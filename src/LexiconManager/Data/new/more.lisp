;;;;
;;;; W::MORE
;;;;

(define-words :pos W::quan :boost-word t
	      :tags (:base500)
	      :words (
		      (W::MORE
		       (wordfeats (W::status ont::indefinite-plural) (W::negatable +) (W::comparative +) (W::Mass ?m))
		       (SENSES
			((LF-PARENT ONT::MORE-VAL) ;; (:* ONT::QMODIFIER W::MORE)) ;; for some reason, LF-PARENT doesn't work here
			 (example "more than seven" "more trucks than that" "more people" "more of the people" "more than seven of the people")
			 (non-hierarchy-lf t)(TEMPL quan-than-comp)
			 (SYNTAX (W::agr (? ag w::3s W::3p))) ;; more than half can still be less than one
			 )
			)
		       )
		      ))

(define-words :pos W::ADV
 :words (
  (W::MORE
   (SENSES
    ((LF-PARENT ONT::degree-modifier)
     (TEMPL PRED-S-POST-TEMPL)
     (example "Eat more.")
     )
    ((LF-PARENT ONT::MORE-VAL)
     ;(TEMPL COMPAR-THAN-templ)
     (TEMPL COMPAR-templ)
     )
    )
   )
  ))


(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::more W::than)
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::MORE-THAN)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

