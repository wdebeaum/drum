;;;;
;;;; W::wanna
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::wanna
   (SENSES
    ((LF-PARENT ONT::WANT)
     (example "I wanna go along the coast")
     (TEMPL experiencer-theme-SUBJCONTROL-TEMPL (xp (% W::VP (W::vform W::base))))
     )
    )
   )
))

