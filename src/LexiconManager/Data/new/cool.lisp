;;;;
;;;; w::cool
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
(w::cool
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::cool)
   (example "cool the sauce")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::cold-scale))
   )
  ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
   (EXAMPLE "the sauce cooled quickly")
   (LF-PARENT ONT::cool)
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::cold-scale))
   (TEMPL affected-unaccusative-templ)
   )
  )
 )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::COOL
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("cool%5:00:00:fashionable:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-RS))
     (LF-PARENT ONT::good)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::med))
     )
    ((meta-data :origin plow :entry-date 20060712 :change-date 20090731 :wn ("cool%3:00:01") :comments plow-req)
      (LF-PARENT ONT::COLD)
      (TEMPL LESS-ADJ-TEMPL)
      )
    )
   )
))

