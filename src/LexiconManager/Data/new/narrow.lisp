
(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (w::narrow
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date 20090504 :comments nil :vn ("other_cos-45.4") :wn ("narrow%2:30:00"))
     (LF-PARENT ONT::decrease)
     (example "narrow the gap")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::NARROW
   (wordfeats (w::comparative +) (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("narrow%3:00:00") :comments html-purchasing-corpus)
     (EXAMPLE "a narrow ridge")
     (lf-parent ont::narrow-val)
     )
    )
   )
))

