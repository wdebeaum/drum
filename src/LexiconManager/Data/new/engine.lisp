;;;;
;;;; W::ENGINE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ENGINE
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
;    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("engine%1:06:01"))
;     (LF-PARENT ONT::vehicle)
;     )
    ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
     (LF-PARENT ONT::machine)
     (example "an engine is a motor that converts thermal energy to mechanical work")
     )
    )
   )
))

