;;;;
;;;; W::lovely
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::lovely
   (wordfeats (W::morph (:FORMS (-er))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :comments nil :wn ("beautiful%3:00:00") :comlex (EXTRAP-ADJ-FOR-TO-INF))
     (example "a lovely book")
     (LF-PARENT ONT::BEAUTIFUL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20090129 :change-date 20090731 :comments nil :wn ("beautiful%3:00:00")  :comlex (ADJ-PP-FOR))
     (example "a thing lovely for him")
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     (LF-PARENT ONT::BEAUTIFUL)
     (TEMPL adj-affected-XP-templ)
     )
    )  
   )
))

