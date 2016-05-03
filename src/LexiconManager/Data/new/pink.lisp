;;;;
;;;; W::PINK
;;;;

(define-words :pos W::n
 :words (
  ((W::PINK W::BEAN)
  (senses
	   ((LF-PARENT ONT::BEANS-PEAS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :words (
   ((w::pink w::salmon)
   (wordfeats (W::morph (:forms (-S-3P) :plur (w::pink w::salmon))))
   (SENSES
    ((meta-data :origin food-kb :entry-date 20050801 :change-date nil :comments nil)
     (LF-PARENT ont::fish)
     )
    )
   )
))

(define-words :pos w::adj :templ central-adj-templ
 :words (
  (W::pink
    (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date 20091027 :comments nil :wn ("pink%3:00:01:chromatic:00"))
     (LF-PARENT ONT::pink)
     (SYNTAX (W::morph (:forms (-er))))
     (templ central-adj-templ)
     )
    )
   )
))

