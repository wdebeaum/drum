;;;;
;;;; W::yield
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::yield
    (wordfeats (W::morph (:forms (-vb) :nom w::yield)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("future_having-13.3") :wn ("yield%2:40:01"))
     (LF-PARENT ONT::surrender)
      (templ agent-affected-goal-optional-templ (xp (% W::pp (W::ptype W::to))))
     ;;(TEMPL agent-affected-goal-optional-templ) ; like grant,offer
      )
    (
     (LF-PARENT ONT::cause-produce-reproduce)
     (TEMPL agent-affected-create-templ)
     )

    )
   )
))

(define-words :pos W::n
 :words (
  (W::yield
   (SENSES
    (
     (LF-PARENT ONT::cause-produce-reproduce)
     (TEMPL COUNT-PRED-TEMPL)
     )

    )
   )
))

