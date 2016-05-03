;;;;
;;;; W::H
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::H w::D)
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :comments calo-y1script)
     (LF-PARENT ont::io-device)
     (example "a 40 gig H D")
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::H
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

