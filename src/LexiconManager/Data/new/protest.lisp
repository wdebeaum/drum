;;;;
;;;; W::protest
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::protest
    (wordfeats (W::morph (:forms (-vb) :nom w::protest)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date 20090508 :comments nil :vn ("conspire-71"))
     ;(LF-PARENT ONT::contest)
     (lf-parent ont::contest-deny-oppose-protest) ;; 20120523 GUM change new parent
     (example "I protest the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

