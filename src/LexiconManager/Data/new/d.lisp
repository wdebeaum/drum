;;;;
;;;; W::D
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::D w::V w::D)
   (SENSES
    ( (meta-data :origin calo :entry-date 20030605 :change-date nil :comments calo-y1script)
     (LF-PARENT ont::data-storage-medium)
     (PREFERENCE 0.96) ; prefer compound if it's dvd drive
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::D W::RAM)
     (wordfeats (W::morph (:forms (-S-3P) :plur (W::d W::rams))))
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::INTERNAL-COMPUTER-STORAGE)
      (lf-form w::dram)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::D
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

