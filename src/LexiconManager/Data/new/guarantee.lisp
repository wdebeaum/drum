;;;;
;;;; W::guarantee
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::guarantee
   (wordfeats (W::morph (:forms (-vb) :past W::guaranteed :ing W::guaranteeing)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20060120 :comments nil :vn ("future_having-13.3") :wn ("guarantee%2:32:01"))
     (LF-PARENT ONT::promise)
     (TEMPL agent-affected-iobj-theme-templ) ; like grant,offer
     )
    ((meta-data :origin step :entry-date 20080705 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::ensure)
     (templ agent-effect-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
     (example "the funding guarantees that the center will stay open")
     )
    )
   )
))

