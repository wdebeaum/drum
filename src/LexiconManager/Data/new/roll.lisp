;;;;
;;;; W::ROLL
;;;;

(define-words :pos W::n
 :words (
  (W::ROLL
  (senses
	   ((LF-PARENT ONT::BAGELS-BISCUITS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::roll
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("roll%2:29:06" "roll%2:38:00"))
     (LF-PARENT ONT::roll)
     (TEMPL affected-templ) ; like move,bounce
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("roll%2:29:06" "roll%2:38:00"))
     (LF-PARENT ONT::rotate)
     (TEMPL agent-affected-xp-templ) ; like rotate,turn,spin
     (PREFERENCE 0.96)
     )
   ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("roll%2:29:06" "roll%2:38:00"))
     (LF-PARENT ONT::roll)
     (TEMPL agent-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    ((example "let's roll with that one")
     (LF-PARENT ONT::SELECT)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-theme-xp-templ (xp (% W::pp (W::ptype W::with))))
     (meta-data :origin calo :entry-date 20050308 :change-date nil :comments computer-purchasing)
     )
    )
   )
))

