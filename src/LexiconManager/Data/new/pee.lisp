;;;;
;;;; W::pee
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pee
   (SENSES
    ((meta-data :origin chf :entry-date 20070828 :change-date nil :comments nil :wn ("urine%1:27:00"))
     (LF-PARENT ONT::bodily-fluid)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
 (W::pee
   (wordfeats (W::morph (:forms (-vb) :past W::peed :ing W::peeing)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070827 :change-date nil :comments chf-dialogues)
     (LF-PARENT ONT::excrete-waste)
     (templ affected-templ)
     )
    )
   )
))

