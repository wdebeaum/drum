;;;;
;;;; W::punc-quotemark
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
(W::punc-quotemark    ;; i.e.,the symbol \"
 (wordfeats (W::morph (:forms (-s-3p) :plur W::punc-quotemark)))
   (SENSES
    ((meta-data :origin calo :entry-date 20060803 :change-date 20070516 :comments nil :wn ("inch%1:23:00"))
     (LF-PARENT ONT::length-unit)
     (LF-FORM W::INCH)
      (TEMPL ATTRIBUTE-UNIT-TEMPL)
      (SYNTAX (W::punc +))
     (preference .97) ;; prefer quotation mark sense
     )
    )
   )
))

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
   (W::punc-quotemark
   (SENSES
    ((LF (W::QUOTE))
     (non-hierarchy-lf t))
    )
   )
))

