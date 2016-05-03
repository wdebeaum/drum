;;;;
;;;; w::white
;;;;

(define-words :pos W::n
 :words (
;; internal
  ((w::white w::blood w::cell)
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::WHITE W::BEAN)
  (senses
	   ((LF-PARENT ONT::BEANS-PEAS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos w::adj :templ central-adj-templ
 :tags (:base500)
 :words (
   (W::white
    (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date 20091027 :comments nil :wn ("white%3:00:01:chromatic:00"))
     (LF-PARENT ONT::white)
     (SYNTAX (W::morph (:forms (-er))))
     (templ central-adj-templ)
     )
    )
   )
))

