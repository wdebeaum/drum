
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::grueling
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090821 :comments nil)
     (EXAMPLE "It's grueling [for him]")
     (lf-parent ont::difficult)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090821 :comments nil)
     (EXAMPLE "it's grueling to do")
     (lf-parent ont::difficult)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

