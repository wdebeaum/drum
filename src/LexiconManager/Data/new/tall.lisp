;;;;
;;;; W::TALL
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::TALL
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("tall%3:00:00"))
     (EXAMPLE "a tall building")
     (LF-PARENT ONT::HEIGHT)
     )
    ;;;;; we want to use the no-premod meaning first
;    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("tall%3:00:00"))
;     (EXAMPLE "a 5 foot tall building")
;     (LF-PARENT ONT::Linear-D)
;     (TEMPL ADJ-PREMOD-TEMPL)
;     (PREFERENCE 0.98)
;     )
    )
   )
))

