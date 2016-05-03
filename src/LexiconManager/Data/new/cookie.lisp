;;;;
;;;; W::COOKIE
;;;;

(define-words :pos W::n
 :words (
  (W::COOKIE
  (senses
	   ((LF-PARENT ONT::COOKIES)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::cookie
   (SENSES
    ((LF-PARENT ONT::information-function-object)
     (EXAMPLE "the site might create a cookie")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("cookie%1:10:00") :comments nil)
     )
    )
   )
))

