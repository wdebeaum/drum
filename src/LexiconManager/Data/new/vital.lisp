;;;;
;;;; w::vital
;;;; 

(define-words :pos W::n
 :words (
  ((W::VITAL W::WHEAT W::GLUTEN)
  (senses
	   ((LF-PARENT ONT::INGREDIENTS)
	    (syntax (W::morph (:forms (-none))))
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::vital
    (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090403 :change-date 20090731 :wn ("crucial%3:00:00") :comments nil)
     (LF-PARENT ONT::necessary)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for))))
     )
    )
   )
))

