;;;;
;;;; W::quality
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::quality
   (SENSES
    ((meta-data :origin calo :entry-date 20031229 :change-date nil :wn ("quality%1:07:02") :comments html-purchasing-corpus)
     (LF-PARENT ONT::evaluation-scale)
;     (SEM (F::scale F::scale))
     (SEM (F::scale (? sc ont::evaluation-scale ont::domain)))
     (TEMPL OTHER-RELN-TEMPL)
     (EXAMPLE "The quality (calibre) of the students has risen")
     )

    ((meta-data :origin domain-reorganization :entry-date 20170808 :change-date nil :wn ("quality%1:07:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::attribute)
     (TEMPL OTHER-RELN-TEMPL)
     (EXAMPLE "he shows strong leadership qualities")
     )
    )
   )
))

