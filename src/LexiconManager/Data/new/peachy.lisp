;;;;
;;;; W::peachy
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::peachy
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20090129 :change-date 20090731 :comments nil :wn ("grand%5:00:00:extraordinary:00" :comlex (EXTRAP-ADJ-FOR-TO-INF-RS)))
     (example "a peachy book")
     (LF-PARENT ONT::good)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL central-adj-templ)
     )))
))

