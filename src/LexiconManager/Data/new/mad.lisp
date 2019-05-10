;;;;
;;;; W::mad
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::mad
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
;    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
;     (example "I am angry / an angry person")
;     (LF-PARENT ONT::angry)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "a mad idea") ;; like crazy 
     (LF-PARENT ONT::insane)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am angry at her")
     (LF-PARENT ONT::angry)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::at w::about w::for)))))
     )
   #|| ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am angry that she does that")
     (LF-PARENT ONT::angry)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )  ||#  
    )
   )
))

