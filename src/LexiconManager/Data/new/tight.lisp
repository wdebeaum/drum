;;;;
;;;; W::TIGHT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::TIGHT
   (wordfeats (W::MORPH (:FORMS (-ly -ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("tight%3:00:01") :comments html-purchasing-corpus)
     (EXAMPLE "a tight fit")
     (LF-PARENT ONT::BINDING-VAL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (w::tight  
   (senses
    ((lf-parent ont::binding-val)
     (example "he held tight")
     (templ PRED-S-POST-templ)
    )))
))




