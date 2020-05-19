;;;;
;;;; W::move
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::move ; no nom
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("move%2:37:00" "move%2:37:01"))
     (LF-PARENT ONT::provoke)
     (example "the story moved him [to change his ways]")
      (TEMPL AGENT-AFFECTED-FORMAL-OBJCONTROL-OPTIONAL-TEMPL) ; like annoy,bother,concern,hurt
      (preference .97)
      )
    ))))

 (define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::move
   (wordfeats (W::morph (:forms (-vb) :nom w::movement)))
   (SENSES   
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("move%2:38:00" "move%2:38:01" "move%2:38:03"))
     (LF-PARENT ONT::MOVE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AFFECTED-TEMPL)
     (example "the truck is moving")
     ;; if this is changed check lou
     (PREFERENCE 0.98) ;; prefer agent interp for intransitive
     )
   
    ((LF-PARENT ONT::cause-move)
     (example "move the cargo to avon")
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    ((meta-data :origin "plow" :entry-date 20060510 :change-date nil :comments nil)
     (LF-PARENT ONT::self-locomote)
     (example "he moved to the corner")
     (TEMPL agent-templ)
     )

    ((LF-PARENT ONT::take-turn)
     (example "Is it my turn to move")
     (preference .985)
     (TEMPL agent-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	  ((w::move w::on)
	   (senses
	    ((LF-PARENT ONT::ACTIVITY-ONGOING)
	     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
	     (example "move on [with the plan/explanation]")
	     (TEMPL AGENT-NEUTRAL-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::with))))
	     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-followup)
	     )
	    ))	  
))

