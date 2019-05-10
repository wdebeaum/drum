;;;;
;;;; W::ANYPLACE
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::ANYPLACE
   (wordfeats (W::else-word +))
   (SENSES
    ((LF-PARENT ONT::AT-LOC)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     )
    )
   )
))


(define-words :pos W::n :templ PPWORD-N-TEMPL
	      :words (
		      (W::ANYPLACE
		       (SENSES
			((LF-PARENT ONT::LOCATION)
			 (PREFERENCE 0.97)
			 )
			)
		       )
		      ))
