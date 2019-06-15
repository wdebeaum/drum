;;;;
;;;; W::heal
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::heal
    (wordfeats (W::morph (:forms (-vb) :ing w::healing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date nil :comments nil :vn ("other_cos-45.4") :wn ("heal%2:30:00" "heal%2:29:00" "heal%2:29:01"))
     (EXAMPLE "The doctor healed my wound")
     (LF-PARENT ONT::CURE)
     )
    
    ((meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date nil :comments nil :vn ("other_cos-45.4") :wn ("heal%2:30:00" "heal%2:29:00" "heal%2:29:01"))
     (EXAMPLE "My wound healed")
     (LF-PARENT ONT::CURE)
     (TEMPL affected-TEMPL)
     )
    )
   )
))

