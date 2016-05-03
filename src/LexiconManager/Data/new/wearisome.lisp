;;;;
;;;; W::wearisome
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::wearisome
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date 20090821 :comments nil :wn ("difficult%3:00:00"))
     (EXAMPLE "It's tiresome [for him]")
     (LF-PARENT ONT::TASK-COMPLEXITY-VAL)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090422 :change-date 20090821 :comments nil :wn ("difficult%3:00:00"))
     (EXAMPLE "it's tiresome to do")
     (LF-PARENT ONT::task-complexity-VAL)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

