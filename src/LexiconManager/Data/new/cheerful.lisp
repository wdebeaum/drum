;;;;
;;;; W::cheerful
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::cheerful
   (wordfeats (W::morph (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "I am cheerful / a cheerful person")
     (LF-PARENT ONT::EUPHORIC)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date 20090731 :comments nil :wn ("happy%3:00:00"))
     (example "the news is cheerful / cheerful news")
     (LF-PARENT ONT::EUPHORIC)
     (templ central-adj-content-templ)
     )
    )
   )
))

