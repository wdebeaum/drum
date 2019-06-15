;;;;
;;;; W::guarantee
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::guarantee
   (wordfeats (W::morph (:forms (-vb) :past W::guaranteed :ing W::guaranteeing)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20060120 :comments nil :vn ("future_having-13.3") :wn ("guarantee%2:32:01"))
     (LF-PARENT ONT::promise)
     (TEMPL AGENT-FORMAL-AFFECTED-TEMPL) ; like grant,offer
     )
    ((meta-data :origin step :entry-date 20080705 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::ensure)
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL (xp (% w::cp (w::ctype w::s-finite))))
     (example "the funding guarantees that the center will stay open")
     )
    )
   )
))

