;;;;
;;;; W::LETTER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::LETTER
   (SENSES
    ((LF-PARENT  ONT::linguistic-object)
     (example "what are the letters of the alphabet")
     )
    ((LF-PARENT ONT::info-medium)
     (example "he wrote a letter to his friend")
     (meta-data :origin calo :entry-date 20041103 :change-date nil :wn ("letter%1:10:00") :comments y2)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::LETTER W::HEAD)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::info-medium)
     )
    )
   )
))

