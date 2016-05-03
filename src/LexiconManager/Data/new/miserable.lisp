;;;;
;;;; W::miserable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::miserable
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :comments LM-vocab)
     (example "I am unhappy / an unhappy person")
     (LF-PARENT ONT::UNHAPPY)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20090129  :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "unhappy news")
     (LF-PARENT ONT::UNHAPPY)
     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090129  :change-date 20090731 :comments LM-vocab)
     (example "I am unhappy about it")
     (LF-PARENT ONT::UNHAPPY)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    #||((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :comments LM-vocab)
     (example "I am unhappy that she does that")
     (LF-PARENT ONT::UNHAPPY)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     ) ||#   
    )
   )
))

