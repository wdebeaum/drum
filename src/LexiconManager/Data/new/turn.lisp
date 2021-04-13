;;;;
;;;; W::turn
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::turn
   (SENSES
    (
     (LF-PARENT ont::action-defined-by-game)
     (example "It's my turn.")
     )
    )
   )
))

;; ;; 20121212 GUM change delete type and associated words
;(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
; :words (
;   ((W::turn w::in)
;   (wordfeats (W::morph (:forms (-vb) :3s w::turns :past W::turned)))
;   (SENSES
;    ((LF-PARENT ONT::prepare-for-sleep)
;     (meta-data :origin cardiac :entry-date 20080828 :change-date nil :comments nil)
;     (TEMPL agent-templ)
;     )
;    )
;   )
;))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::TURN
   (wordfeats (W::morph (:forms (-vb) :ing W::turning :nom w::turn)))
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("turn%2:38:01" "turn%2:38:02" "turn%2:38:04" "turn%2:38:13"))
     (LF-PARENT ONT::rotate)
     (TEMPL affected-templ) ; like move,bounce but use ont::rotate (more specific) instead of ont::move
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("turn%2:38:01" "turn%2:38:02" "turn%2:38:04" "turn%2:38:13"))
     (LF-PARENT ONT::ROTATE)
     (example "turn the car (to the right)")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((LF-PARENT ONT::ROTATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "turn (to the right)")
     (TEMPL AGENT-TEMPL)
     )

   ;; GUM change 20121027 new word sense as per LF_GUM spreadsheet
    ((LF-PARENT ONT::TRANSFORMATION)
     ;(TEMPL AGENT-AFFECTED-RESULT-XP-NP-TEMPL (xp (% w::pp (w::ptype (? w::into)))))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "turn x into y")
     )
    ((LF-PARENT ONT::TRANSFORMATION)
     (TEMPL AFFECTED-TEMPL)
     (example "x turned into y")
     )
    ((LF-PARENT ONT::TRANSFORMATION)
     (TEMPL AFFECTED-FORMAL-XP-PRED-TEMPL)
     (example "It turned green")
     )
   )
)))

#|
(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::turn (W::off))
   (wordfeats (W::morph (:forms (-vb) :ing W::turning)))
   (SENSES
    ((LF-PARENT ont::turn-off)
     (example "turn off the radio")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ;; turning off things like water means really turning off whatever source produces them
    ((LF-PARENT ont::turn-off)
     (example "turn off the water") 
     (templ agent-affected-xp-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (preference 0.95) ;; to interpret turn it off primarily as agent-affected
     )
    ))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::turn (W::on))
   (wordfeats (W::morph (:forms (-vb) :ing W::turning)))
   (SENSES
    ((LF-PARENT ont::start-object)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ))
  ))
|#

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::turn w::out)
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("occurrence-48.3"))
     (LF-PARENT ONT::have-property)
     (example "it happens to be true")
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL (xp (% w::cp (w::ctype w::s-to)))) ; like happen
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("occurrence-48.3"))
     (LF-PARENT ONT::occurring)
     (TEMPL neutral-templ) ; like occur,happen
     )
    )
   )

))

