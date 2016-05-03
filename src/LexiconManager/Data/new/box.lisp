;;;;
;;;; W::box
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::box
   (SENSES
    ((meta-data :origin fruitcarts :entry-date 20050225 :change-date nil :wn ("box%1:06:00") :comments nil)
     ;(LF-PARENT ONT::small-container)
     (lf-parent ont::box)
     (example "paint the box red")
     (TEMPL pred-subcat-contents-templ)
     )
   ))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
;; jr 20120713 moving text-field sense to an untagged definition to remove this sense from the :base500 used in BOLT
 :words (
   (W::box
   (SENSES
    ((meta-data :origin plow :entry-date 20050318 :change-date nil :comments nil)
     (example "put the name in the text box")
     (LF-PARENT ONT::text-field)
     (preference .98)
     )
    )
   )
))

