;;;;
;;;; W::throb
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::throb
   (wordfeats (W::morph (:forms (-vb) :nom W::throb)))
   (SENSES
    ((LF-PARENT ONT::rhythmic-motion)
     (example "his heart throbbed")
     (TEMPL affected-templ)
     (meta-data :origin cardiac :entry-date 20081215 :change-date nil :comments LM-vocab)
     )
    )
   )
))

