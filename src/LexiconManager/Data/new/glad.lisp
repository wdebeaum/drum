;;;;
;;;; W::GLAD
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::GLAD
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
;    ((meta-data :origin chf :entry-date 20070809 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
;     (example "I am glad")
;     (LF-PARENT ONT::GRATEFUL)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "glad tidings")
     (LF-PARENT ONT::GRATEFUL)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
  ((meta-data :origin chf :entry-date 20070809 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am glad for her")
     (LF-PARENT ONT::GRATEFUL)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for))))
     )
 #|| ((meta-data :origin chf :entry-date 20070809 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am glad that she does that")
     (LF-PARENT ONT::GRATEFUL)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     ) ||#   
    )
   )
))

