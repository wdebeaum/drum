;;;;
;;;; W::chat
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::chat
   (wordfeats (W::morph (:forms (-vb) :nom w::chat)))
   (SENSES
    (;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::schmooze-talk)
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("chat%2:32:00"))
     (example "he chatted") ;maybe... --wdebeaum
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    (;;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::schmooze-talk)
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     (example "he chatted with her [about it]")
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp w::to w::with)))))
     )
    (;;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::schmooze-talk)
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     (example "he chatted about it [to/with her]")
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL)
     )
    ))
))

