;;;;
;;;; W::CALORIE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CALORIE
   (SENSES
    ;; this is not quite right... calorie measures heat, not temperature --wdebeaum
    ;; yup... changed
    ((LF-PARENT ONT::energy-unit) 
     (TEMPL attribute-unit-TEMPL)
     (example "how many calories are in a serving of carrots")
     (META-DATA :ORIGIN nutrition :ENTRY-DATE 20050707 :CHANGE-DATE NIL
      :COMMENTS nil))))
))

