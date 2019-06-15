;;;;
;;;; W::recruit
;;;;

(define-words :pos W::v 
 :words (
  (W::recruit
   (wordfeats (W::morph (:forms (-vb) :nom w::recruitment)))
   (SENSES
    ((lf-parent ont::enroll)
     (example "recruit the basketball player")
     (meta-data :origin "verbnet-2.0" :entry-date 20060605 :change-date nil :comments nil :vn ("orphan-29.7") :wn ("recruit%2:40:00" "recruit%2:33:00"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory F::-))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

