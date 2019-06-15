;;;;
;;;; W::rotate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::rotate
    (wordfeats (W::morph (:forms (-vb) :nom W::rotation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("rotate%2:38:00" "rotate%2:38:01"))
     (LF-PARENT ONT::rotate)
     (TEMPL affected-templ) ; like move,bounce but use ont::rotate (more specific) instead of ont::move
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::ROTATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("rotate%2:38:00" "rotate%2:38:01"))
     (example "rotate the triangle to the left")
     )
    ((EXAMPLE "rotate")
     (LF-PARENT ONT::rotate)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-templ)
     (meta-data :origin fruit-carts :entry-date 20050422 :change-date nil :comments nil)
     )
    )
   )
))

