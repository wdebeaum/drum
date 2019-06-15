;;;;
;;;; W::discover
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::discover
   (wordfeats (W::morph (:forms (-vb) :past W::discovered :ing w::discovering :nom w::discovery)))
   (SENSES
    ((LF-PARENT ONT::determine)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-that))))
     (example "he discovered that the route is not longer passable")
     (meta-data :origin bee :entry-date 20040609 :change-date nil :comments portability-expt :vn ("discover-84"))
     )
    ((LF-PARENT ONT::come-to-understand)
     (example "he discovered John in the kitchen")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    ((LF-PARENT ONT::determine)
     (example "he discovered how to do it")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::NP (W::sort W::wh-desc))))
     (meta-data :origin plow :entry-date 20060714 :change-date nil :comments caloy3)
     )
    )
   )
))

