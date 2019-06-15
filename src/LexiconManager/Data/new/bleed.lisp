;;;;
;;;; W::bleed
;;;;

(define-words :pos W::v 
 :words (
  (W::bleed
   (wordfeats (W::morph (:forms (-vb) :past W::bled :nom W::bleed)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090529 :comments nil :vn ("cheat-10.6") :wn ("bleed%2:30:00" "bleed%2:40:09"))
     (LF-PARENT ONT::take-incrementally)
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-OF-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::of))))
     ;(TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::of))))
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::bleed)
     (TEMPL affected-TEMPL)
     (EXAMPLE "She bled profusely")
     )
    )
   )
))

