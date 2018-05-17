;;;;
;;;; W::stock
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::stock w::exchange)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060714 :change-date nil :wn ("stock_exchange%1:06:00") :comments caloy3)
     (LF-PARENT ONT::financial-institution)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::stock
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-goal-affected-templ (xp (% w::pp (w::ptype (? t w::with))))) ; like load
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("keep-15.2") :wn ("stock%2:40:00"))
     (LF-PARENT ONT::put-away)
     (example "do you stock organic building supplies")
 ; like keep
     )
    )
   )
))

