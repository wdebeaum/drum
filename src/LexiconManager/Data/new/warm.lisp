;;;;
;;;; w::warm
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
  (w::warm
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date 20090504 :comments nil :vn ("other_cos-45.4") :wn ("warm%2:30:00" "warm%2:30:01"))
     (LF-PARENT ONT::heat)
     (example "warm the water")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::warm
   (wordfeats (W::MORPH (:FORMS (-ER -ly))))
   (SENSES
    ((meta-data :origin plow :entry-date 20060713 :change-date 20090731 :wn ("warm%3:00:01") :comments plow-req)
      (LF-PARENT ONT::WARM)
      (TEMPL LESS-ADJ-TEMPL)
      )
    ((meta-data :origin step :entry-date  20081027  :change-date nil :comments nil)
     (example "the atmosphere is warm / a warm atmosphere")
     (LF-PARENT ONT::social-interaction-VAL)
     (templ central-adj-templ)
     )
    )
   )
))

