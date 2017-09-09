;;;;
;;;; W::NECESSARY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::NECESSARY
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("necessary%3:00:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::necessary)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for))))
     )
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("necessary%3:00:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::necessary)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("sufficient%3:00:00"))
     (EXAMPLE "This is necessary to buy the cow.")
     ;(LF-PARENT ONT::ADEQUATE)
     (LF-PARENT ONT::necessary)
;     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::for))))
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::CP (W::ctype W::s-to))))
     )    
    )
   )
))

