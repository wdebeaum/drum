;;;;
;;;; W::hurt
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::hurt
   (wordfeats (W::morph (:forms (-vb) :past W::hurt)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090515 :comments nil :vn ("amuse-31.1") :wn ("hurt%2:30:04"))
     (EXAMPLE "Light hurts my eyes")
     (LF-PARENT ONT::evoke-hurt)
     (TEMPL agent-affected-xp-templ)
     )
   ((meta-data :origin cardiac :entry-date 20081215 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("hurt%2:30:04"))
     (EXAMPLE "his leg hurts")
     ;(LF-PARENT ONT::experiencer-obj)
     (LF-PARENT ONT::PHYSICAL-SENSATION)
     (TEMPL experiencer-TEMPL)
     )
   )
   )
))

