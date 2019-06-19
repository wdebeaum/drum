;;;;
;;;; W::FOOT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
(W::FOOT
   (wordfeats (W::morph (:forms (-s-3p) :plur W::feet)))
   (abbrev w::ft)
   (SENSES
    ((meta-data :origin calo :entry-date 20060803 :change-date nil :comments nil :wn ("foot%1:23:00"))
     (LF-PARENT ONT::length-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::FOOT
   (wordfeats (W::morph (:forms (-s-3p) :plur W::feet)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("foot%1:08:01"))
     (LF-PARENT ONT::external-body-part)
     (preference .985)  ; prefer the UNIT readings as they can be eliminated easily by context
     (TEMPL BODY-PART-RELN-TEMPL)
     )
    )
   )
))

