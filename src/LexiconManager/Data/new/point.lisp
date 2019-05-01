;;;;
;;;; W::POINT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::POINT
   (SENSES
    ((LF-PARENT ONT::shape)
     (example "the flag with the point on it")
     (meta-data :origin fruitcarts :entry-date 20050401 :change-date nil :wn ("point%1:25:02") :comments fruitcarts-03-3)
     )
    ((LF-PARENT ONT::referential-sem)
     (example "a five point scale")
     (meta-data :origin plow :entry-date 20060523 :change-date nil :comments pq)
     (preference .96)
     )
    )
   )
))

(define-words :pos W::v 
 :tags (:base500)
 :words (
 (W::point
   (SENSES
    ((LF-PARENT ONT::pointing-to)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ (xp (% W::PP (W::ptype (? pt W::toward w::towards))))) 
     (example "the triangle points towards the square")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Orient)
     )
    ((LF-PARENT ONT::orient)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-TEMPL) ;; (xp (% W::PP (W::ptype (? pt W::toward w::towards)))))
     (example "orient the triangle (towards the square)")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Orient)
     )
    )
   )
))

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::POINT
   (SENSES
    ((LF (W::DECIMAL-POINT))
     (non-hierarchy-lf t)(SYNTAX (W::punctype W::decimalpoint))
     )
    )
   )
))

