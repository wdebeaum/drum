;;;;
;;;; w::big
;;;;

(define-words :pos W::n
 :words (
  ((w::big w::mac)
  (senses
	   ((LF-PARENT ONT::FAST-FOOD)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::BIG
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("big%3:00:01"))
     (LF-PARENT ONT::large)
     (SEM (F::GRADABILITY +) (f::orientation F::pos) (f::intensity ont::med))
     )
    )
   )
))

