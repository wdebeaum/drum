
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::subordinate
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (meta-data :origin calo-ontology :entry-date 20060714 :change-date nil :wn ("subordinate%1:18:00") :comments caloy3)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::subordinate
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("subordinate%5:00:00:junior:00") :comments html-purchasing-corpus :comlex (ADJECTIVE))
      (example "a good book")
      (lf-parent ont::substandard-val)
      (TEMPL central-adj-templ)
      )
     )
    )
))

