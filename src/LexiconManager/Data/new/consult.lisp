;;;;
;;;; W::consult
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::consult
   (wordfeats (W::morph (:forms (-vb) :nom w::consultation)))
   (SENSES
    ((LF-PARENT ONT::interview)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL)
     (example "consult him about it")
     (meta-data :origin task-learning :entry-date 20060512 :change-date nil :comments nil :vn ("battle-36.4" "inquire-37.1.2" "meet-36.3-1") :wn ("consult%2:32:02" "consult%2:32:01" "consult%2:32:03"))
     )
      ;; have to have this straight transitive sense for passive to work
    ((LF-PARENT ONT::interview)
     (example "consult the boss" "he was consulted")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-addressee-templ)
     )
    ;; consult the manual
    )
   )
))

