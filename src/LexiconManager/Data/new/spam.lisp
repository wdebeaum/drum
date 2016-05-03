;;;;
;;;; W::SPAM
;;;;

(define-words :pos W::n
 :words (
  (W::SPAM
  (senses
	   ((LF-PARENT ONT::MEAT-OTHER)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::spam
   (SENSES
    ((LF-PARENT ONT::email)
     (EXAMPLE "Too much of my legitimate email is getting marked as spam")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("spam%1:10:00") :comments nil)
     )
    )
   )
))

