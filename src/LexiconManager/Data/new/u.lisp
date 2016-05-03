;;;;
;;;; W::U
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    ((W::U w::R w::L)
    (SENSES
     ((EXAMPLE "click on the url")
      (meta-data :origin calo :entry-date 20041004 :change-date nil :comments caloy2)
      (LF-PARENT ONT::symbolic-representation)
      )
     )
    )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::U
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

