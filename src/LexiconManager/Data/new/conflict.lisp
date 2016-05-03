;;;;
;;;; W::conflict
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::conflict
   (wordfeats (W::morph (:forms (-vb) :nom w::conflict)))
   (SENSES
    ((LF-PARENT ONT::HINDERING)
     (Example "a plan conflicts with another")
     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-expt)
     (TEMPL agent-affected-xp-templ (xp (% W::PP (W::ptype W::with))))
     )    
     ((LF-PARENT ONT::HINDERING)
      (example "2 plans conflict")
      (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-expt)
      (TEMPL AFFECTED-plural-TEMPL)
      )
     ))
))

