;;;;
;;;; w::opinion
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::opinion
   (senses
    ((lf-parent ont::mental-object)
     (example "everyone is entitled to their opinion")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :wn ("opinion%1:09:00") :comments caloy3)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt w::on W::about)))))
     )	   	   	   
    ))
))

