;;;;
;;;; W::want
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::want
   (SENSES
    ((LF-PARENT ONT::WANT)
     (TEMPL experiencer-neutral-templ)
     (EXAMPLE "I want a dog")
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL experiencer-theme-subjcontrol-templ)
     (EXAMPLE "I want to go")
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL experiencer-action-OBJCONTROL-TEMPL)
     (EXAMPLE "I want you to go")
     )
    )
   )
))

