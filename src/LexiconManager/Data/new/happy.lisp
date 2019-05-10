;;;;
;;;; W::HAPPY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::HAPPY
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
;    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
;     (example "I am happy / a happy person")
;     (LF-PARENT ONT::EUPHORIC)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "the news is happy / happy news")
     (LF-PARENT ONT::EUPHORIC)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    ((meta-data :origin leam :entry-date 20070214 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am happy for her")
     (LF-PARENT ONT::EUPHORIC)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for))))
     )
    ;; now handled using general principles
    #||((meta-data :origin leam :entry-date 20070214 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am happy that she does that")
     (LF-PARENT ONT::EUPHORIC)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )||#   
    )
   )
))

