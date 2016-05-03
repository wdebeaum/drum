;;;;
;;;; W::permit
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::permit
   (SENSES
    ((EXAMPLE "Permit him to go")
     (LF-PARENT ONT::ALLOW)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-affected-objcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin boudreaux :entry-date 20031024 :vn ("allow-64"))
     )
    ((example "the advances permitted greater flexibility")
     (sem (f::aspect f::dynamic))
     (templ agent-affected-xp-templ)
     (meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
     (LF-PARENT ONT::allow)
     )
    )
   )
))

