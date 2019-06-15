;;;;
;;;; W::TIP
;;;;

(define-words :pos W::n
 :words (
  ((W::TIP W::ROUND)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::tip
   (SENSES
    ((LF-PARENT ONT::information)
     (EXAMPLE "You can accomplish many tasks using these iCal tips and tricks")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("tip%1:10:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::tip
   (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date nil :comments nil)
     (LF-PARENT ONT::fall)
     (example "I started to tip over")
     (TEMPL affected-TEMPL)
     )
    )
   )
))

