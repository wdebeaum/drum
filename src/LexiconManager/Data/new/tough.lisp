;;;;
;;;; W::TOUGH
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::TOUGH
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090821 :wn ("tough%5:00:01:difficult:00") :comments html-purchasing-corpus)
     (EXAMPLE "that's tough [for him]")
     (LF-PARENT ONT::difficult)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("tough%5:00:01:difficult:00"))
     (EXAMPLE "it's tough to do")
     (LF-PARENT ONT::difficult)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

