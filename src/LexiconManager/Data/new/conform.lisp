;;;;
;;;; w::conform
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::conform
      (wordfeats (W::morph (:forms (-vb) :nom w::conformation)))
    (senses
     ((LF-PARENT ONT::Compliance)
      (example "conform to the procedure")
      (templ agent-theme-xp-templ (xp (% w::PP (w::ptype w::to))))
      (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil)
      )
     ))
))

