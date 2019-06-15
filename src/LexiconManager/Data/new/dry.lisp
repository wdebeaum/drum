;;;;
;;;; W::dry
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
;   )
    (W::dry
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (lf-parent ont::dry-val)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ;; a dry cough
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::dry
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "dry the clothes")
     (LF-PARENT ONT::dry)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::dry-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "clothes dried quickly")
     (LF-PARENT ONT::dry)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::dry-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))
