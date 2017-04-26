;;;;
;;;; W::off
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::off w::kilter)
   (SENSES
    ((LF-PARENT ONT::defective-val)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::off w::balance)
   (SENSES
    ((LF-PARENT ONT::unsteady)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  ((w::off w::kilter)
  (senses
   ((LF-PARENT ONT::strange)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :wn ("strange%3:00:00") :comments nil)
    )
   )
)
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
	  (w::off
	   (senses
	    (;(lf-parent ont::artifact-property-val)
	     (lf-parent ont::inactive-off)
	     ;(templ central-adj-templ)
	     (templ predicative-only-adj-templ)
	     (Example "The switch is off -- predicative only, because 'the off switch' is not at all the same")
	     (meta-data :origin bee :entry-date 20040408 :change-date nil :wn ("off%3:00:00") :comments test-s)
	     )
	    ))
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::OFF
   (SENSES
    ((LF-PARENT ONT::DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    ((LF-PARENT ONT::off)
     (example "it is off the table")
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::OFF
   (SENSES
    ((LF (W::OFF))
     (non-hierarchy-lf t))
    )
   )
))

