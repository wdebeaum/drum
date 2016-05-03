;;;;
;;;; W::memory
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::memory W::stick)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments caloy3)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MEMORY
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("memory%1:06:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::internal-computer-storage)
     (templ mass-pred-templ)
     )
    )
   )
))

