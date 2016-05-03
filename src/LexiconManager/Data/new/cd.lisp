;;;;
;;;; W::CD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CD
   (SENSES
    ((meta-data :origin allison :entry-date 20040922 :change-date nil :wn ("cd%1:06:00") :comments caloy2)
     (LF-PARENT ont::data-storage-medium)
     (example "I want to be able to burn cds")
     (PREFERENCE 0.96) ; prefer compound if it's cd rom drive
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::CD W::ROM)
   (SENSES
    ((meta-data :origin calo :entry-date 20040622 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::internal-computer-storage)
     (example "I need a cd rom")
     )
    )
   )
))

