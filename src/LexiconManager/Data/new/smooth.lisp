;;;;
;;;; W::SMOOTH
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::SMOOTH
    (wordfeats (W::MORPH (:FORMS (-ER -LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("smooth%3:00:00") :comments html-purchasing-corpus)
      (LF-PARENT ONT::smooth-val)
      (TEMPL LESS-ADJ-TEMPL)
      )
     )
    )
))


(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::smooth
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "smooth(en) the surface")
     (LF-PARENT ONT::smoothen)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::smoothness-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "the skin smoothed over time")
     (LF-PARENT ONT::smoothen)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::smoothness-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))
