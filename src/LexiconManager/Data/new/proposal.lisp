;;;;
;;;; w::proposal
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::proposal
   (senses
    ((lf-parent ont::proposal)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype W::for))))
     (example "submit a proposal for a project")
     (meta-data :origin calo :entry-date 20050328 :change-date 20050817 :wn ("proposal%1:10:00") :comments lf-restructuring)
     )	   	   	   	   
    ))
))

