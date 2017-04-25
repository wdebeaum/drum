;;;;
;;;; W::WIDE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::WIDE
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031222 :change-date 20090731 :wn ("wide%3:00:00") :comments html-purchasing-cfellorpus)
     (EXAMPLE "a wide road")
     (LF-PARENT ONT::BROAD)
     )
    ;;;;; we want to use the no-premod meaning first
;    ((meta-data :origin calo :entry-date 20031222 :change-date nil :wn ("wide%3:00:00") :comments html-purchasing-corpus)
;     (EXAMPLE "a 5 foot wide road")
;     (LF-PARENT ONT::linear-D)
;     (TEMPL ADJ-PREMOD-TEMPL)
;     (PREFERENCE 0.98)
;     )
    )
   )
))

