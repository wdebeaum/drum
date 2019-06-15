;;;;
;;;; W::DOCK
;;;;

(define-words :pos W::n
 :words (
  (W::DOCK
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::dock
   (SENSES
    ((LF-PARENT ONT::computer-software)
     (EXAMPLE "click the icon in the Dock")
     (meta-data :origin task-learning :entry-date 20050822 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::dock
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("pocket-9.10-1"))
     (LF-PARENT ONT::arrive)
     (TEMPL affected-result-xp-templ) ; like land
     )
    )
   )
))

