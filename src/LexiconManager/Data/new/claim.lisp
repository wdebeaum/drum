;;;;
;;;; W::claim
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::claim
    (wordfeats (W::morph (:forms (-vb) :nom w::claim)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("claim%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::assert)
     (example "he claimed his innocence to me")
     (TEMPL agent-theme-to-addressee-templ)
     )
    (;;(LF-parent ont::announce)
     (lf-parent ont::assert)
     (Example "He claimed to know the answer")
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin "verbnet-1.5-corrected" :entry-date 20060214 :change-date 20090506 :comments nil :vn ("say-37.7") :wn ("claim%2:32:00")    )
     )
    (;;(LF-PARENT ONT::talk)
     (lf-parent ont::assert)
     (example "He claims that he knows the answer")
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-to W::s-finite)))))
     (meta-data :origin step :entry-date 20080627 :change-date nil :comments nil :vn ("say-37.7") :wn ("claim%2:32:00"))
     )
    ((LF-PARENT ONT::appropriate)
     (example "He claimed his bags")
     (TEMPL AGENT-affected-XP-TEMPL)
     (meta-data :origin step :entry-date 20080627 :change-date nil :comments nil :vn ("say-37.7") :wn ("claim%2:32:00"))
     )
    ((LF-PARENT ONT::assert)
     (example "I claimed it to be broken")
     (TEMPL agent-effect-affected-objcontrol-templ)
    )
    )
    
   )
))

