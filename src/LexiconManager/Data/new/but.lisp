;;;;
;;;; w::but
;;;;

#||(define-words :pos w::adv
  :words (
	  ((w::but w::not)
	   (senses
	    ((TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	     (LF-PARENT ONT::BUT-EXCEPTION)
	     (preference .98) ;; prefer the ADV operator when possible
	     )
	    ((TEMPL  NEG-ADJ-ADV-OPERATOR-TEMPL)
	     (LF-PARENT ONT::BUT-EXCEPTION)
	     (LF-FORM W::BUT-NOT)
	     )
	    )
	   )
	  ))||#

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  ((W::BUT w::not)
   (wordfeats (w::but-not +))
   (SENSES
    ((LF (:* ONT::BUT-EXCEPTION W::BUT-NOT))
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-ANY-TEMPL)
     )
    )
   )
  
  ))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::BUT
   (SENSES
    ((LF-PARENT ONT::CONJUNCT)
     )
    )
   )
))

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::BUT
   (wordfeats (W::conj +) (w::but +))
   (SENSES
    ((LF ONT::BUT)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-ANY-TEMPL)
     )
    )
   )
  
  ))



