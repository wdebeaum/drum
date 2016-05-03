;;;;
;;;; W::indicate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::indicate
   (wordfeats (W::morph (:forms (-vb) :nom W::indication)))
   (SENSES
    ((LF-PARENT ONT::correlation)
     (example "a cough indicates a cold")
     (TEMPL neutral-neutral-templ)
     )
    ((LF-PARENT ONT::correlation)
     (example "a cough indicates whether a person has a cold")
     (TEMPL neutral-theme-xp-templ (xp (% w::cp (w::ctype (? ct w::s-finite)))))
     )
    )
   )
))

