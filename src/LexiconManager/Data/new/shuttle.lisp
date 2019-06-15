;;;;
;;;; W::shuttle
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::shuttle
   (SENSES
    ((LF-PARENT ONT::land-vehicle)
     (meta-data :origin calo-ontology :entry-date 20060523 :change-date nil :wn ("shuttle%1:06:01") :comments pq0404)
     (example "is there a hotel shuttle from the airport")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::shuttle
   (SENSES
    ;; note that here the goal is not a subcat but a spatial adverbial modifier
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("drive-11.5-1"))
     (LF-PARENT ONT::TRANSPORT)
     (example "shuttle the cargo to avon")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
   
   )
)))

