;;;;
;;;; W::farm
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::farm
   (SENSES
   ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
    ;(LF-PARENT ONT::facility)
    (LF-PARENT ONT::farm)
    (example "wind energy is used on farms")
    )
   #|
    ((meta-data :origin step :entry-date 20080721 :change-date nil :comments step6)
    (LF-PARENT ONT::organization)
    (example "the farm made a profit")
     )
   |#
    )
   )
))

