;;;;
;;;; W::SERIES
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SERIES
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090520 :comments html-purchasing-corpus)
     (LF-PARENT ONT::sequence)
     (TEMPL other-reln-templ)
     )
    )
   )
))
#|
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SERIES
   (SENSES
    ((meta-data :origin beetle2 :entry-date 20081704 :change-date nil :comments nil :wn nil)
     (example "a series circuit" "in series")
     (LF-PARENT ONT::orientation-val)     
     (TEMPL central-adj-TEMPL)
     (preference 0.98)
     )
    )
   )
))|#

