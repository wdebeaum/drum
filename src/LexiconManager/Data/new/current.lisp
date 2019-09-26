;;;;
;;;; W::CURRENT
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CURRENT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("current%1:19:01"))
     (LF-PARENT ONT::substance)
     (SEM (f::form f::wave))
     (TEMPL COUNT-PRED-TEMPL)
     )
    )
   )
))
|#

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::current
	   (senses
	    ((lf-parent ont::electric-measure-scale)
	     (syntax (w::mass w::mass))
	     (meta-data :origin bee :entry-date 20040407 :change-date nil :wn ("current%1:19:01") :comments test-s)
	     )
	    ))
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::CURRENT
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("current%3:00:00"))
     (lf-parent ont::now)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

