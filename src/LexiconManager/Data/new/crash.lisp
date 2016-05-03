;;;;
;;;; W::crash
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::crash
    (wordfeats (W::morph (:forms (-vb) :nom w::crash)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("crash%2:35:00" "crash%2:35:01" "crash%2:35:02" "crash%2:38:02"))
     (LF-PARENT ont::break-object)
     (TEMPL agent-affected-xp-templ) ; like break,crack,fracture,rip,shatter,chip,splinter,snap
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::BREAK-OBJECT)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2 :vn ("break-45.1") :wn ("crash%2:35:00" "crash%2:35:01" "crash%2:35:02" "crash%2:38:02"))
     (example "the computer crashed")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     )
    #||((LF-PARENT ONT::BREAK-OBJECT)   ;; subsumed by CAUSE template
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he crashed the computer")
     )||#
    ((LF-PARENT ont::break-object)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     
     (TEMPL agent-affected-xp-templ)
     (example "it crashed the computer")
     )
    )
   )
))

