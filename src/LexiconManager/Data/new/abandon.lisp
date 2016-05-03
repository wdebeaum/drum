;;;;
;;;; W::abandon
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::abandon
   (wordfeats (W::morph (:forms (-vb) :past W::abandoned :ing W::abandoning :nom w::abandonment)))
   (SENSES
    ((LF-PARENT ONT::LEAVE-behind)
     (example "he abandoned his dog at the house")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("leave-51.2") :wn ("abandon%2:40:00" "abandon%2:38:00"))
     )
    )
   )
))

