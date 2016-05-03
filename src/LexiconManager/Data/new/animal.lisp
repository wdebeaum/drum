;;;;
;;;; W::ANIMAL
;;;;

(define-words :pos W::n
 :words (
  ((W::ANIMAL W::CRACKER)
  (senses
	   ((LF-PARENT ONT::COOKIES)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::animal
   (SENSES
    ((LF-PARENT ONT::animal)
     (EXAMPLE "man is an animal")
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

