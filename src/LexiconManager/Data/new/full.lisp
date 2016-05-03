;;;;
;;;; W::FULL
;;;;

(define-words :pos W::n
 :words (
  ((W::FULL W::CUT)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::FULL
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("full%3:00:00"))
     (LF-PARENT ONT::FULL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::full
   (SENSES
    ((LF-PARENT ont::degree-modifier-VERYHIGH)
     (LF-FORM W::full)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "pan camera full left")
     (meta-data :origin coordops :entry-date 20070514 :change-date nil :comments nil)
     )
    ((LF-PARENT ont::degree-modifier-VERYHIGH)
     (LF-FORM W::full)
     (TEMPL PRED-VP-TEMPL)
     (example "zoom full")
     (meta-data :origin coordops :entry-date 20070514 :change-date nil :comments nil)
     )
    )
   )
))

