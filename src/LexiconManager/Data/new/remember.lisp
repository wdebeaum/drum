;;;;
;;;; W::remember
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::remember
   (wordfeats (W::morph (:forms (-vb) :past W::remembered :ing w::remembering :nom w::remembrance)))
   (SENSES
    ((LF-PARENT ONT::REMEMBER)
     (meta-data :origin calo :entry-date 20051219 :change-date nil :comments nil :wn ("remember%2:31:00"))
     (example "remember that every cloud has a silver lining")
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::REMEMBER)
     (example "he remembers everything")
     (meta-data :origin calo :entry-date 20051219 :change-date nil :comments nil :wn ("remember%2:31:00"))
     (TEMPL agent-theme-xp-templ)
     )
    ((LF-PARENT ONT::remember)
     (meta-data :origin calo :entry-date 20051219 :change-date nil :comments nil :wn ("remember%2:31:00"))
     (example "remember to take your prinivil")
     (TEMPL agent-theme-subjcontrol-templ)
     )
    ((LF-PARENT ONT::remember)
     (example "I remember now")     
     (meta-data :origin leam :entry-date 20070201 :change-date nil :comments lam-demo2  :wn ("remember%2:31:00"))
     (TEMPL agent-templ)
     (preference 0.95)
     )    
    )
   )
))

