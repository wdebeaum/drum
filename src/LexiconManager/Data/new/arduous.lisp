;;;;
;;;; W::arduous
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::arduous
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date 20090731 :comments 20090821 :wn ("difficult%3:00:00"))
     (EXAMPLE "It's tiresome [for him]")
     (LF-PARENT ONT::difficult)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090422 :change-date 20090731 :comments 20090821 :wn ("difficult%3:00:00"))
     (EXAMPLE "it's tiresome to do")
     (LF-PARENT ONT::difficult)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

