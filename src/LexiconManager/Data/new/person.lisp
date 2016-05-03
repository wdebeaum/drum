;;;;
;;;; W::PERSON
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PERSON
   (wordfeats (W::morph (:forms (-s-3p) :plur W::people)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("person%1:03:00"))
     (LF-PARENT ONT::PERSON)
     )
    )
   )
))

