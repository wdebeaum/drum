;;;;
;;;; W::foretell
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::foretell
   (wordfeats (W::morph (:forms (-vb) :past W::foretold :ing W::foretelling)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::predict)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like realize
     )
    )
   )
))

