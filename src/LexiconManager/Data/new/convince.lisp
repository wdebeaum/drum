;;;;
;;;; W::convince
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::convince
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090602 :change-date nil :comments nil)
     (LF-PARENT ONT::convince)
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL (xp (% W::cp (W::ctype (? ct W::s-finite w::s-to)))))
     (example "convince me that I should stay")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090602 :change-date nil :comments nil)
     (LF-PARENT ONT::convince)
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::of))))
     (example "convince me of its value")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090602 :change-date nil :comments nil)
     (LF-PARENT ONT::convince)
     (templ agent-addressee-theme-objcontrol-req-templ)
     (example "he convinced me to stay")
     )
    )
   )
))

