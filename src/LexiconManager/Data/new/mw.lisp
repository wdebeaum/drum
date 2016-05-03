;;;;
;;;; W::mw
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ;; need to distinguish betwee mW milliwatt and MW megawatt
   (W::mw
    (wordfeats (W::morph (:forms (-s-3p) :plur W::mw)))
   (SENSES
    ((meta-data :origin calo :entry-date 20041130 :change-date nil :wn ("milliwatt%1:23:00") :comments caloy2)
     (LF-PARENT ONT::power-unit)
     (TEMPL SUBSTANCE-UNIT-TEMPL)
     (lf-form w::megawatt)
     (example "a milliwatt is about one thousandth of a watt")
     )
    )
   )
))

