;;;;
;;;; W::progress
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::progress
   (wordfeats (W::morph (:forms (-vb) :nom W::progression)))
   (SENSES
    ((EXAMPLE "The disease progressed")
     (LF-PARENT ONT::progress)
     (TEMPL affected-templ)
     )
    )
   )
))

