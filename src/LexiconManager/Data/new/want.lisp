;;;;
;;;; W::want
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::want
   (SENSES
    ((LF-PARENT ONT::WANT)
     (TEMPL experiencer-neutral-templ)
     (EXAMPLE "I want a dog")
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL)
     (EXAMPLE "I want to go")
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-A-TEMPL)
     (EXAMPLE "I want you to go")
     )
    )
   )
))

