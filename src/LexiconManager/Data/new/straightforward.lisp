;;;;
;;;; W::STRAIGHTFORWARD
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::STRAIGHTFORWARD
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090821 :wn ("straightforward%5:00:00:unequivocal:00") :comments html-purchasing-corpus)
     (EXAMPLE "that's straightforward [for him]")
     (lf-parent ont::straightforward-val)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("straightforward%5:00:00:unequivocal:00"))
     (EXAMPLE "it's straightforward to explain")
     (lf-parent ont::straightforward-val)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

