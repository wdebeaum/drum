;;;;
;;;; W::THOSE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::THOSE
   (wordfeats (W::WH (? WH W::R -)) (W::agr W::3p))
   (SENSES
    ;;;;;;	 ((sem (origin (? !n f_human)))
    ;;;;;;	 (LF-PARENT LF_ANY-PHYS-OBJECT))
    ;;;;;;	 ((LF-PARENT LF_ANY-ABSTRACT-OBJECT))
    ;;;;;;	 ((LF-PARENT LF_SITUATION-ROOT))
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SYNTAX (w::status w::pro-set))
     (SEM (F::origin (? !n F::human)))
     )
    )
   )
))

(define-words :pos W::art :boost-word t
 :tags (:base500)
 :words (
  (W::THOSE
   (wordfeats (W::agr W::3p) (W::diectic +) (W::mass (? mass W::COUNT W::BARE)))
   (SENSES
    ((LF W::DEFINITE-plural)
     (non-hierarchy-lf t)(TEMPL mass-agr-TEMPL)
     )
    )
   )
))

