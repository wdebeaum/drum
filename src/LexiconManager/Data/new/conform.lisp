;;;;
;;;; w::conform
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::conform
      (wordfeats (W::morph (:forms (-vb) :nom w::conformation)))
    (senses
     ((LF-PARENT ONT::Compliance)
      (example "he conformed to the procedure")
      (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::PP (w::ptype w::to))))
      (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil)
     )

     ((example "the goods must conform to their description")
      (LF-PARENT ONT::RESEMBLE)
      (meta-data :wn ("conform%2:42:06"))
      (templ neutral-neutral1-xp-templ (xp (% w::PP (w::ptype w::to))))
      )

     ))
))

