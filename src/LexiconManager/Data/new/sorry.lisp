;;;;
;;;; W::sorry
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
 (W::sorry
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
;    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
;     (example "I am unhappy / an unhappy person")
;     (LF-PARENT ONT::sorry)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "unhappy news")
     (LF-PARENT ONT::sorry)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (example "I am unhappy about it")
     (LF-PARENT ONT::sorry)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    #||((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (example "I am unhappy that she does that")
     (LF-PARENT ONT::sorry)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )   ||# 
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::sorry
   (SENSES
    ((LF (W::SORRY))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_APOLOGIZE))
     )
    )
   )
))

