;;;;
;;;; W::jam
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::jam
   (SENSES
    ((LF-PARENT ONT::problem)
     (TEMPL count-pred-TEMPL)
     (example "I'm in a jam")
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :wn ("jam%1:26:00") :comments meeting-understanding)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::jam
  (senses
	   ((LF-PARENT ONT::CONDIMENTS)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::jam
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-affected-goal-optional-templ (xp (% w::pp (w::ptype (? t w::on w::into w::in))))) ; like pack
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-goal-affected-templ (xp (% w::pp (w::ptype (? t w::with))))) ; like pack
     )
    
    )
   )
))

