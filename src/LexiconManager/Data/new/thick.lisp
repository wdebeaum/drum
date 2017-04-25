;;;;
;;;; W::THICK
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::THICK
   (wordfeats (w::comparative +)(W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031222 :change-date 20090731 :wn ("thick%3:00:01") :comments html-purchasing-corpus)
     (EXAMPLE "a thick wall")
     (lf-parent ont::thick-val)
     )
    ;;;;; we want to use the no-premod meaning first
;    ((meta-data :origin calo :entry-date 20031222 :change-date nil :wn ("thick%3:00:01") :comments html-purchasing-corpus)
;     (EXAMPLE "a 5 foot thick wall")
;     (LF-PARENT ONT::linear-D)
;     (TEMPL ADJ-PREMOD-TEMPL)
;     (PREFERENCE 0.98)
;     )
    )
   )
))

