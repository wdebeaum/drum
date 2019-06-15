;;;;
;;;; W::destroy
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::destroy
   (wordfeats (W::morph (:forms (-vb) :nom w::destruction)))
   (SENSES
    ((meta-data :origin step :entry-date 20080705 :change-date nil :comments nil)
     (LF-PARENT ONT::destroy)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "overwatering destroyed the plants")
     )
    )
   )
))

