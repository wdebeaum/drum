;;;;
;;;; W::thankful
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::thankful
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am thankful / a thankful person")
     (LF-PARENT ONT::GRATEFUL)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "a thankful expression") ;; not a stimulus
     (LF-PARENT ONT::GRATEFUL)
     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am thankful for her")
     (LF-PARENT ONT::GRATEFUL)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for))))
     )
    #||((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am thankful that she does that")
     (LF-PARENT ONT::GRATEFUL)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )  ||#  
    )
   )
))

