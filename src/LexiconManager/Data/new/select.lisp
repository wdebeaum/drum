;;;;
;;;; W::select
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::select
   (SENSES
    ((meta-data :origin calo :entry-date 20060414 :change-date nil :comments nil :vn ("characterize-29.2") :wn ("select%2:31:00"))
     (LF-PARENT ONT::SELECT)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "select that plan/option")
     )
    ((meta-data :origin plow :entry-date 20060531 :change-date nil :comments nil :vn ("characterize-29.2") :wn ("select%2:31:00"))
     (LF-PARENT ONT::SELECT)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "select red")
     (templ agent-theme-pred-templ)
     )
    )
   )
))

