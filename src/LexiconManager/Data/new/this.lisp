;;;;
;;;; W::THIS
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::THIS
   (wordfeats (W::pointer +) (W::sing-lf-only +))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SEM (F::origin (? !n F::human)))
     )
    )
   )
))

(define-words :pos W::art :boost-word t
 :tags (:base500)
 :words (
  (W::THIS
   (wordfeats (W::agr W::3s) (W::diectic +))
   (SENSES
    ((LF W::DEFINITE)
     (non-hierarchy-lf t)(TEMPL mass-agr-TEMPL)
     )
    )
   )
))

