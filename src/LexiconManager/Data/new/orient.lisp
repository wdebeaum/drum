;;;;
;;;; W::orient
;;;;

(define-words :pos W::v
 :words (
 (W::orient
   (SENSES
    #||((LF-PARENT ONT::orient)   ;; isn't this one the passive
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-theme-xp-templ (xp (% W::PP (W::ptype (? pt W::toward w::towards)))))
     (example "the triangle is oriented towards the square")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Orient)
     )||#
    ((LF-PARENT ONT::orient)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     (TEMPL agent-affected-goal-to-TEMPL (xp (% W::PP (W::ptype (? pt W::toward w::towards)))))
     (example "orient the triangle (towards the square)")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Orient)
     )
    )
   )
))

