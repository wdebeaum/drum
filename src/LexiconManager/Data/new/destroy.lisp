;;;;
;;;; W::destroy
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
 (W::destroy
   (wordfeats (W::morph (:forms (-vb) :nom w::destruction)))
   (SENSES
    ((meta-data :origin step :entry-date 20080705 :change-date nil :comments nil)
     (LF-PARENT ONT::destroy)
     (templ agent-affected-xp-templ)
     (example "overwatering destroyed the plants")
     )
    )
   )
))

