;;;;
;;;; W::TOMATO
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TOMATO
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::tomatoes))))
   (SENSES
    ((LF-PARENT ONT::fruit) ;; a fruit, but used as a veggie
     (meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :wn ("tomato%1:13:00") :comments nil)
     (example "take a tomato")
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::TOMATO W::BEEF W::WITH W::NOODLE)
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::TOMATO W::RICE)
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

