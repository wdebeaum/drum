;;;;
;;;; W::GRAPEFRUIT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ;; same entry. We cannot have 2 different entries because it breaks the lexicon tool.
  (W::GRAPEFRUIT
   (SENSES
    ((LF-PARENT ONT::fruit)
     (syntax (W::morph (:forms (-none))))
     (meta-data :origin fruit-carts :entry-date 20050426 :change-date nil :wn ("grapefruit%1:13:00") :comments nil)
     (example "take two grapefruit")
     (TEMPL COUNT-PRED-3p-TEMPL)
     (PREFERENCE 0.98)
     )
    ((LF-PARENT ONT::fruit)
     (meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :wn ("grapefruit%1:13:00") :comments nil)
     (example "take a grapefruit")
     (syntax (W::morph (:forms (-S-3P))))
     )
    )
   )
))

