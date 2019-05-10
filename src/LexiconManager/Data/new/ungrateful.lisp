;;;;
;;;; w::ungrateful
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ungrateful
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
;    ((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
;     (example "he is ungrateful / an ungrateful person")
;     (lf-parent ont::not-grateful-val)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "a ungrateful comment") ;; not a stimulus
     (lf-parent ont::not-grateful-val)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    #||((meta-data :origin cardiac :entry-date 20080414 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am ungrateful that she does that")
     (lf-parent ont::not-grateful-val)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )  ||#  
    )
   )
))

