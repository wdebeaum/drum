;;;;
;;;; W::MESSAGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MESSAGE
   (SENSES
    ((LF-PARENT ONT::object-containing-message)
     (example "the message that he couldn't come")
     (templ count-subcat-that-optional-templ)
     (meta-data :origin calo :entry-date 20041130 :change-date nil :wn ("message%1:10:00") :comments caloy2 :wn-sense (1 2))
     )
    )
   )
))

