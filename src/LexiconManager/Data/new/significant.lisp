;;;;
;;;; W::significant
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::significant w::other)
    (wordfeats (W::morph (:forms (-s-3p) :plur (w::significant w::others))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("significant_other%1:18:00") :comments nil)
     (LF-PARENT ONT::family-relation)
     (templ part-of-reln-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SIGNIFICANT
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("significant%3:00:00"))
     (LF-PARENT ONT::primary)
     (EXAMPLE "A significant change in tax laws.")
     )
    ((meta-data :origin adjective-reorganization :entry-date 20170413 :change-date nil :comments nil :wn ("significant%5:00:00:considerable:00"))
     (LF-PARENT ONT::substantial-val)
     (EXAMPLE "He won by a significant margin.")
     )
    )
   )
))

