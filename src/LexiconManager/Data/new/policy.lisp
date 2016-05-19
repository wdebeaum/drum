;;;;
;;;; w::policy
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::policy
   (senses
    ((lf-parent ont::policy)
     (templ other-reln-formal-templ (xp (% W::pp (W::ptype (? pt W::for)))))
     (example "the policy for buying computers")
     (meta-data :origin calo :entry-date 20050324 :change-date 20050817 :wn ("policy%1:09:00") :comments lf-restructuring)
     )
    ((lf-parent ont::policy)
     (templ other-reln-formal-templ (xp (% W::cp (W::ctype W::s-to))))
     (example "the policy to close the park")
     )
    ))
))

