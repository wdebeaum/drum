;;;;
;;;; W::restrict
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::restrict
   (wordfeats (W::morph (:forms (-vb) :nom w::restriction)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070810 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     (example "have you been following the restricted plan")
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It restricts him from doing something")
     )
    )
   )
))

