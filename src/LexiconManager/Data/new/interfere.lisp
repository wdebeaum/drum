;;;;
;;;; W::interfere
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::interfere
   (wordfeats (W::morph (:forms (-vb) :nom W::interference)))
   (SENSES
    ((LF-PARENT ONT::HINDERING)
     (Example "a drug interferes with aspirin")
     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-expt)
     (TEMPL agent-affected-xp-templ (xp (% W::PP (W::ptype W::with))))
     )
    ))
))

