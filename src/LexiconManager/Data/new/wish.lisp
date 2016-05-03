;;;;
;;;; W::wish
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::wish
   (SENSES
    ((LF-PARENT ONT::WANT)
     (TEMPL experiencer-action-SUBJCONTROL-TEMPL)
     (EXAMPLE "I wish to copy part of the code")
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil )
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (W::ctype W::s-that))))
     (EXAMPLE "I wish (that) we could have spaghetti for dinner")
     (meta-data :origin task-learning :entry-date 20060208 :change-date nil  :vn ("wish-62"))
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL experiencer-neutral-xp-templ (xp (% W::pp (W::ptype W::for))))
     (EXAMPLE "I wish for spaghetti")
     (meta-data :origin task-learning :entry-date 20060208 :change-date nil  :vn ("wish-62"))
     )
    )
   )
))

