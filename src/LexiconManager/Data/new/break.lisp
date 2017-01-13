;;;;
;;;; w::break
;;;;

#|
(define-words :pos W::n
 :words (
  (w::break
  (senses
	   ((LF-PARENT ONT::event)
	    (TEMPL COUNT-PRED-TEMPL)
	    (meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments nil)
	    )
	   )
  ;; crack of the whip, crack of dawn
)
))
|#

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::break w::down)
   (wordfeats (W::morph (:forms (-vb) :past w::broke :pastpart W::broken :nom (w::break w::down))))
   (SENSES
    ((LF-PARENT ONT::fail)
     (SYNTAX (w::resultative +))
     (example "he broke down and ate french fries")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-TEMPL)
     (meta-data :origin chf :entry-date 20070827 :change-date nil :comments chf-dialogues)
     )
    ))
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::break
   (wordfeats (W::morph (:forms (-vb) :past W::broke :pastpart W::broken :ing W::breaking :nom w::break)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090601 :comments nil :vn ("cheat-10.6") :wn ("break%2:42:04"))
     (LF-PARENT ONT::remove-from)
     (TEMPL agent-source-affected-optional-templ)
     (PREFERENCE 0.96)
     (example "She finally broke herself of smoking cigarettes")
     )
    ((LF-PARENT ONT::BREAK-OBJECT)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2 :vn ("break-45.1") :wn ("break%2:29:04" "break%2:30:00" "break%2:30:10" "break%2:30:15" "break%2:35:00" "break%2:38:11" "break%2:41:08"))
     (example "the window broke")
     (SYNTAX (w::resultative +))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AFFECTED-TEMPL)
     )
    ((LF-PARENT ONT::BREAK-OBJECT)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he broke the window")
     )
   
    ((LF-PARENT ONT::render-ineffective)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2 :vn ("break-45.1") :wn ("break%2:29:04" "break%2:30:00" "break%2:30:05" "break%2:30:10" "break%2:30:15" "break%2:35:00" "break%2:38:11" "break%2:41:08"))
     (example "the browser broke")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     (SYNTAX (w::resultative +))
     )
    ((LF-PARENT ONT::render-ineffective)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (SEM (F::Aspect F::unbounded))
     (TEMPL agent-affected-xp-templ)
     (example "he broke the browser")
     )
    
       )
)))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
;   )
  ((W::break (W::up))
   (wordfeats (W::morph (:forms (-vb) :past W::broke :pastpart W::broken :nom (w::break w::up))))
   (SENSES
    ((LF-PARENT ONT::Separation)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     )
    )
   )
))

