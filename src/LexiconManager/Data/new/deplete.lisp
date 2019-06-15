;;;;
;;;; W::deplete
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::deplete
   (wordfeats (W::morph (:forms (-vb) :nom w::depletion)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090601 :comments nil :vn ("cheat-10.6") :wn ("deplete%2:34:00"))
     ;(LF-PARENT ONT::empty)
     ;(TEMPL agent-source-affected-optional-templ)
     (LF-PARENT ONT::DECREASE-completely)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

