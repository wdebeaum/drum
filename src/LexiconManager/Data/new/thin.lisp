;;;;
;;;; w::thin
;;;; 

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::thin
 (senses
  ((meta-data :origin cardiac :entry-date 20090417 :change-date nil :comments nil)
   (LF-PARENT ONT::adjust)
   (example "thin the sauce")
   (templ agent-affected-xp-templ)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
    )
  ((meta-data :origin cardiac :entry-date 20090417 :change-date nil :comments nil)
   (LF-PARENT ONT::adjust)
   (example "his hair thinned")
   (templ affected-templ)
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
     (EXAMPLE "a thin line" "a thin person")
     (lf-parent ont::thin-val)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::less))
     )
   )
   )
))

