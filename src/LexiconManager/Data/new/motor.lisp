;;;;
;;;; W::motor
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::motor
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
     (LF-PARENT ONT::machine)
     (example "a motor is a machine that converts other forms of energy into mechanical energy and so imparts motion")
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::motor
  (senses
   ((LF-PARENT ONT::body-system-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

