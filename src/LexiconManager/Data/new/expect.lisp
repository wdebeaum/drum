;;;;
;;;; W::expect
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::expect
   (wordfeats (W::morph (:forms (-vb) :nom w::expectation)))
   (SENSES
    ((LF-PARENT ONT::EXPECTATION)
     (example "what side effects should I expect")
     (TEMPL experiencer-theme-xp-templ)
     )
    ((LF-PARENT ONT::expectation)
     (example "he expects to have a degree next year")
     (meta-data :origin calo :entry-date 20040907 :change-date nil :comments caloy2 :vn ("wish-62") :wn ("expect%2:31:00"))
     (TEMPL experiencer-action-SUBJCONTROL-TEMPL)
     )
    ((LF-PARENT ONT::EXPECTATION)
     (example "she expects him to have a degree next year")
     (meta-data :origin calo :entry-date 20040907 :change-date nil :comments caloy2 :vn ("wish-62")  :wn ("expect%2:31:00"))
     (TEMPL experiencer-ACTION-OBJCONTROL-TEMPL)
     )
    ((meta-data :origin calo-ontology :entry-date 20060315 :change-date nil :comments nil  :wn ("expect%2:31:00"))
     (LF-PARENT ONT::expectation)
     (example "he expects that he will do it")
     (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype (? cp w::s-finite)))))
     )
    )
   )
))

