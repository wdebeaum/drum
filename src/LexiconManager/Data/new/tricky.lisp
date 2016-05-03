;;;;
;;;; W::TRICKY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::TRICKY
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("tricky%5:00:00:difficult:00"))
     (EXAMPLE "that's tricky [for him]")
     (LF-PARENT ONT::difficult)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("tricky%5:00:00:difficult:00"))
     (EXAMPLE "the car is tricky to repair")
     (LF-PARENT ONT::difficult)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

