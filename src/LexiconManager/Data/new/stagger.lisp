;;;;
;;;; W::stagger
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::stagger
   (wordfeats (W::morph (:forms (-vb) :past W::staggered :ing W::staggering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("stagger%2:37:00"))
     (LF-PARENT ont::cause-body-effect)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("stagger%2:38:00" "stagger%2:38:01" "stagger%2:38:02"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

