;;;;
;;;; W::grateful
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::grateful
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am grateful / a grateful person")
     (LF-PARENT ONT::GRATEFUL)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "a grateful note") ;; not a stimulus
     (LF-PARENT ONT::GRATEFUL)
     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am grateful for her")
     (LF-PARENT ONT::GRATEFUL)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for))))
     )
    #||((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am grateful that she does that")
     (LF-PARENT ONT::GRATEFUL)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     ) ||#
    )
   )
))

