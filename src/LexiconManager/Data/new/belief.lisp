;;;;
;;;; w::belief
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::belief
   (senses
    ((lf-parent ont::mental-object)
     (example "he has strong beliefs about that")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :wn ("belief%1:09:01") :comments caloy3)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt w::in W::about)))))
     )	   	   	   
    ))
))

