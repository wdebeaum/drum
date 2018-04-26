;;;;
;;;; W::relationship
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::relationship
   (SENSES
    ((meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     (lf-parent ont::relation)
     (example "the relationship bewteen unemployment and political attitudes")
     (templ reln-between-neutral-templ)
     )
    ((meta-data :origin cause-result-relations :entry-date 20180410 :change-date nil :comments projector-purchasing)
     (lf-parent ont::social-relationship)
     (example "Joe has a strong relationship with his family")
     )
    )
   )
))

