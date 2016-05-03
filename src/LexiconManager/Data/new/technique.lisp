;;;;
;;;; w::technique
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::technique
   (senses
    ((lf-parent ont::procedure)
     (templ other-reln-templ (xp (% W::pp (W::ptype (? pt W::for)))))
     (example "the technique for solving this problem")
     (meta-data :origin calo :entry-date 20040621 :change-date 20050817 :wn ("technique%1:09:00") :comments lf-restructuring)
     )	   	   	   	   
    ))
))

