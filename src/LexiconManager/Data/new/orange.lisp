;;;;
;;;; W::ORANGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::ORANGE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("orange%1:13:00"))
     (LF-PARENT ONT::fruit)
     )
    )
   )
))


(define-words :pos W::n
 :words (
  ((W::ORANGE W::ROUGHY)
  (senses
	   ((LF-PARENT ONT::SALTWATER-fish)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos w::adj :templ central-adj-templ
 :tags (:base500)
 :words (
    (W::orange
    (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date 20091027 :comments nil :wn ("orange%3:00:01:chromatic:00"))
     (LF-PARENT ONT::orange)
     (SYNTAX (W::morph (:forms (-er))))
     (templ central-adj-templ)
     )
    )
   )
))

