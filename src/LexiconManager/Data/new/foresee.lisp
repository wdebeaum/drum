;;;;
;;;; W::foresee
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::foresee
   (wordfeats (W::morph (:forms (-vb) :past W::foresaw :pastpart W::foreseen :ing W::foreseeing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::expectation)
     (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like realize
     )
    )
   )
))

