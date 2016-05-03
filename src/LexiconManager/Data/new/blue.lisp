;;;;
;;;; W::BLUE
;;;;

(define-words :pos W::n
 :words (
  ((W::BLUE W::CHEESE)
  (senses
	   ((LF-PARENT ONT::CHEESE)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::BLUE W::MUSSEL)
  (senses
	   ((LF-PARENT ONT::MOLLUSKS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos w::adj :templ central-adj-templ
 :tags (:base500)
 :words (
   (W::blue
    (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date 20091027 :comments nil :wn ("blue%3:00:01:chromatic:00"))
     (LF-PARENT ONT::blue)
     (SYNTAX (W::morph (:forms (-er))))
     (templ central-adj-templ)
     )
    )
   )
))

