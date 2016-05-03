;;;;
;;;; W::V
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::V W::RAM)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::v W::rams))))
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::INTERNAL-COMPUTER-STORAGE)
      (lf-form w::vram)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::V
    (wordfeats (W::morph (:forms (-s-3p) :plur W::v)))
    (SENSES
     ((meta-data :origin bee :entry-date 20040805 :change-date nil :wn ("v%1:23:00") :comments portability-followup)
      ;; note that v is plural there, it's equivalent to "5 volts"
      (example "5V")
      (LF-PARENT ONT::power-unit)
      (lf-form w::volt)
      (TEMPL SUBSTANCE-UNIT-TEMPL)
      )
     )
    )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::V
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ) 
    )
   )
)
))

