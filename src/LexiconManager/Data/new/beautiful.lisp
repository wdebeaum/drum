;;;;
;;;; W::BEAUTIFUL
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::BEAUTIFUL
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("beautiful%3:00:00") :comlex (EXTRAP-ADJ-FOR-TO-INF))
     (example "a beautiful book")
     (LF-PARENT ONT::BEAUTIFUL)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("beautiful%3:00:00")  :comlex (ADJ-PP-FOR))
     (example "a person beautiful for him")
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::hi))
     (LF-PARENT ONT::BEAUTIFUL)
     (TEMPL adj-affected-XP-templ)
     )
    )  
   )  
))

