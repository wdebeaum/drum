;;;;
;;;; W::minutes
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::minutes
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::chronicle)
     (example "the minutes of the meeting")
     (meta-data :origin plow :entry-date 20060524 :change-date nil :wn ("minutes%1:10:00") :comments pq0134)
     (templ count-pred-3p-templ)
     )
    )
   )
))

