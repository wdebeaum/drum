;;;;
;;;; w::thought
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::thought
   (senses
    ((lf-parent ont::concept-notion)
     (example "the thought entered my mind")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :wn ("thought%1:09:01") :comments caloy3)
     )	   	   	   
    ((lf-parent ont::opinion)
     (example "what are your thoughts/opinion on the subject")
     (meta-data :origin domain-reorganization :entry-date 20170904 :change-date nil :wn ("thought%1:09:03") :comments caloy3)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt w::on W::about)))))
     )
    ))
))

