;;;;
;;;; W::digest
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::digest
   (wordfeats (W::morph (:forms (-vb) :nom W::digestion)))
   (SENSES
    ((EXAMPLE "he digested the meal")
     (LF-PARENT ONT::digest)
     (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
     (TEMPL AFFECTED-AFFECTED1-XP-NP-TEMPL)
     )
    ((EXAMPLE "he digested in silence")
     (LF-PARENT ONT::digest)
     (TEMPL affected-TEMPL)
     )
    ;((LF-PARENT ONT::CONSUME)
     ;(TEMPL agent-affected-xp-TEMPL)
;     )
    )
   )
))

