;;;;
;;;; W::strip
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::strip
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090528 :comments nil :vn ("cheat-10.6") :wn ("strip%2:30:05" "strip%2:30:06" "strip%2:30:12" "strip%2:35:04" "strip%2:40:00" "strip%2:40:03"))
     (LF-PARENT ONT::pull-off)
     (TEMPL agent-source-affected-optional-templ)
     (example "strip the wire of insulation" "strip the officer of his rank")
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090528 :comments nil :vn ("cheat-10.6") :wn ("strip%2:30:05" "strip%2:30:06" "strip%2:30:12" "strip%2:35:04" "strip%2:40:00" "strip%2:40:03"))
     (LF-PARENT ONT::pull-off)
     (TEMPL agent-affected-source-optional-templ)
     (example "strip the insulation from the wire")
     )
    )
   )
))

