;;;;
;;;; W::foreknow
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::foreknow
   (wordfeats (W::morph (:forms (-vb) :past W::foreknew :pastpart W::foreknown :ing W::foreknowing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::expectation)
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like realize
     )
    )
   )
))

