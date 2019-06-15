;;;;
;;;; W::afford
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::afford
   (wordfeats (W::morph (:forms NIL)) (W::vform W::base))
   (SENSES
    ((meta-data :origin calo :entry-date 20040420 :change-date 20090508 :comments caloy1v4)
     (LF-PARENT ONT::afford)
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL)
     (example "he can afford 5 dollars")
     )
   ((meta-data :origin calo :entry-date 20040420 :change-date 20090508 :comments caloy1v4)
     (LF-PARENT ONT::afford)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (example "he can afford that computer")
     )
    ((meta-data :origin calo :entry-date 20040420 :change-date 20090508 :comments caloy1v4)
     (LF-PARENT ONT::afford)
     (TEMPL NEUTRAL-FORMAL-CP-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (example "he can afford to spend five dollars")
     )
    )
   )
))

