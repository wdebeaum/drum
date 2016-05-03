;;;;
;;;; W::presume
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::presume
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (LF-PARENT ONT::ASSUME)
       (meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("presume%2:31:00"))
     (EXAMPLE "I presume (that) he knows what he's doing")
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (LF-PARENT ONT::ASSUME)
       (meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("presume%2:31:00"))
     (TEMPL experiencer-templ)
     )
    )
   )
))

