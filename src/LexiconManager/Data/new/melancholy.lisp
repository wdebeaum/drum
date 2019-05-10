;;;;
;;;; W::melancholy
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::melancholy
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
;    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
;     (example "I am melancholy / a melancholy person")
;     (LF-PARENT ONT::unhappy)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "a melancholy outlook")
     (LF-PARENT ONT::unhappy)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (example "I am melancholy about it")
     (LF-PARENT ONT::unhappy)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    #||((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (example "I am melancholy that she does that")
     (LF-PARENT ONT::unhappy)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     ) ||#   
    )
   )
))

