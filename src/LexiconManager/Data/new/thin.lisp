;;;;
;;;; w::thin
;;;; 

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::thin
 (senses
  ((meta-data :origin cardiac :entry-date 20090417 :change-date nil :comments nil)
   (LF-PARENT ONT::thin)
   (example "thin the sauce")
   (templ agent-affected-xp-templ)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::texture-thinness-scale))
    )
  ((meta-data :origin cardiac :entry-date 20090417 :change-date nil :comments nil)
   (LF-PARENT ONT::thin)
   (example "the sauce thinned quickly with oil")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::texture-thinness-scale))
   (templ affected-unaccusative-templ)
   )
  ((LF-PARENT ONT::flatten)
   (meta-data :origin cause-result-relations :entry-date 20180907 :change-date nil :comments nil)
   (templ agent-affected-xp-templ)
   (example "She thinned the metal with a hammer.")
   (PREFERENCE 0.98)
   )
 )
 )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::THIN
   (wordfeats (w::comparative +) (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("thin%3:00:01") :comments html-purchasing-corpus)
     (EXAMPLE "a thin line")
     (lf-parent ont::thin-val)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::less))
     )
    ((meta-data :origin domain-reorganization :entry-date 20171002 :change-date nil :wn ("thin%3:00:03"))
     (EXAMPLE "a thin person")
     (lf-parent ont::skinny-val)
     )
   )
   )
))

