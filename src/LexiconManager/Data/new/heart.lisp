;;;;
;;;; W::heart
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::heart
   (SENSES
    ((meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :wn ("heart%1:25:00") :comments nil)
     (example "take the square with the heart on the side")
     (LF-PARENT ONT::SHAPE-OBJECT)
     (preference .98) ;; prefer body part
     )
    ((LF-PARENT ONT::internal-body-part)
     (meta-data :origin medadvisor :entry-date nil :change-date 20041103 :wn ("heart%1:08:00") :comments nil)
     (example "I have an irregular heart beat")
     (templ body-part-reln-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::HEART W::OF W::PALM)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

