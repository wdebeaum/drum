;;;;
;;;; w::red
;;;;

(define-words :pos W::n
 :words (
;; internal
  ((w::red w::blood w::cell)
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::RED W::LEAF W::LETTUCE)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos w::adj :templ central-adj-templ
 :tags (:base500)
 :words (
   (W::red
    (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date 20091027 :comments nil :wn ("red%3:00:01:chromatic:00"))
     (LF-PARENT ONT::red)
     (SYNTAX (W::morph (:forms (-er))))
     (templ central-adj-templ)
     )
    )
   )
))

