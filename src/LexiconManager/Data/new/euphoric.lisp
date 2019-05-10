;;;;
;;;; W::euphoric
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::euphoric
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
;    ((meta-data :origin cardiac :entry-date 20090427 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
;     (example "I am happy / a happy person")
;     (LF-PARENT ONT::EUPHORIC)
;     (templ central-adj-experiencer-templ)
;     )
     ((meta-data :origin cardiac :entry-date 20090427 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am happy for her")
     (LF-PARENT ONT::EUPHORIC)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for))))
     )
    ((meta-data :origin cardiac :entry-date 20090427 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am happy that she does that")
     (LF-PARENT ONT::EUPHORIC)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )    
    )
   )
))

