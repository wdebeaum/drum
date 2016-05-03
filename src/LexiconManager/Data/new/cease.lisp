;;;;
;;;; W::cease
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::cease
   (wordfeats (W::morph (:forms (-vb) :nom W::cessation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20070323 :comments nil :vn ("begin-55.1-3"))
     (LF-PARENT ONT::stop)
     (example "the party ceased")
     (TEMPL affected-templ) ; like end
     )
    ((meta-data :origin csli-ts :entry-date 20070323 :change-date nil :comments nil)
     (LF-PARENT ONT::stop)
     (example "the managers ceased to work")
     (TEMPL agent-effect-subjcontrol-templ (xp (% w::cp (w::ctype w::s-to))))
     )
    ((meta-data :origin csli-ts :entry-date 20070323 :change-date nil :comments nil)
     (LF-PARENT ONT::stop)
     (example "the managers ceased working")
     (TEMPL agent-effect-subjcontrol-templ (xp (% w::vp (w::vform w::ing))))
     )
    ((meta-data :origin csli-ts :entry-date 20070323 :change-date nil :comments nil)
     (LF-PARENT ONT::stop)
     (example "cease the process")
     (templ agent-effect-xp-templ)
     )
    )
   )
))

