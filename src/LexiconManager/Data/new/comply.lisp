;;;;
;;;; w::comply
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::comply
    (wordfeats (W::morph (:forms (-vb) :nom w::compliance)))
    (senses
     ((LF-PARENT ONT::Compliance)
      (example "comply with the laws")
      (templ agent-theme-xp-templ (xp (% w::PP (w::ptype w::with))))
      (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
      )
     ))
))

