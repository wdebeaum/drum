;;;;
;;;; W::collective
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::collective
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050831 :change-date 20090915 :wn ("collective%3:00:00") :comments nil)
     (EXAMPLE "control the distribution of derivative or collective works")
     (LF-PARENT ONT::collective-VAL)
     (TEMPL central-adj-plur-templ)
     )
    )
   )
))

