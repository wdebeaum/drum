;;;;
;;;; W::SAD
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SAD
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
;    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("sad%3:00:00") :comments html-purchasing-corpus)
;     (LF-PARENT ONT::unhappy)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "a sad day")
     (LF-PARENT ONT::sad-val)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    ((meta-data :origin leam :entry-date 20070214 :change-date 20090731 :comments nil :wn ("sad%3:00:00"))
     (example "I am happy for her")
     (LF-PARENT ONT::sad-val)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::for w::about)))))
     )
    #||((meta-data :origin leam :entry-date 20070214 :change-date 20090731 :comments nil :wn ("sad%3:00:00"))
     (example "I am happy that she does that")
     (LF-PARENT ONT::unhappy)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )
    ||#
   ))
))

