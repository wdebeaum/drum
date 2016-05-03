;;;;
;;;; W::inhale
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::inhale
     (wordfeats (W::morph (:forms (-vb) :nom w::inhalation)))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-affected-xp-templ)
     (example "he inhaled the air")
     )
     ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
      (LF-PARENT ONT::bodily-process)
     (TEMPL agent-templ) ; like bleed
     )
    )
   )
))

