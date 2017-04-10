
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::IMPOSSIBLE
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("impossible%3:00:00"))
     (EXAMPLE "that's impossible [for him]")
     (lf-parent ont::not-possible-val)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("impossible%3:00:00"))
     (EXAMPLE "it's impossible to do")
     (lf-parent ont::not-possible-val)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

