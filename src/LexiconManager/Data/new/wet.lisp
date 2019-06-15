;;;;
;;;; W::wet
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::wet
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (lf-parent ont::wet-val)
     (templ central-adj-templ)
     )
    )
   )
))

(define-words :pos W::V
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::wet
 (senses
  ((meta-data :origin cause-result-relations :entry-date 20190108 :change-date nil :comments nil)
   (LF-PARENT ONT::dampen)
   (example "Wet your face.")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
    )
  )
 )
))