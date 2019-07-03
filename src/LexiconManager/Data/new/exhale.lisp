;;;;
;;;; W::exhale
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
      (W::exhale
       (wordfeats (W::morph (:forms (-vb) :nom w::exhalation)))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::exhale)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "he exhaled smoke")
     )
     ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
      (LF-PARENT ONT::exhale)
     (TEMPL agent-templ) ; like bleed
     )
    )
   )
))

