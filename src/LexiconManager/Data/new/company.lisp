;;;;
;;;; W::COMPANY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::COMPANY
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20090520 :comments nil)
     (LF-PARENT ONT::military-group)
     (example "a company of soldiers")
     (TEMPL classifier-count-pl-templ)
     (PREFERENCE 0.97)
     )
    ((meta-data :origin calo :entry-date 20040113 :change-date nil :wn ("company%1:14:01") :comments calo-y1script)
     (LF-PARENT ONT::company)
     (example "a software company")
     (templ count-pred-templ)
     )
    )
   )
))

