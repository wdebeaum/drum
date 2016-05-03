;;;;
;;;; W::make
;;;;

(define-words :pos W::v :templ agent-affected-create-templ
 :tags (:base500)
 :words (
  (W::make
   (wordfeats (W::morph (:forms (-vb) :past W::made)))
   (SENSES
    ((EXAMPLE "Let's make a new plan")
     (LF-PARENT ONT::CREATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin trains :entry-date unknown :change-date nil :comments swier)
     )
    ((EXAMPLE "make money" "make a profit")
     (LF-PARENT ONT::earning) 
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-xp-TEMPL)
     (meta-data :origin step :entry-date 20081022 :change-date nil :comments step6)
     )
    ((LF-PARENT ONT::transformation) ;; GUM change new parent 20121027
     (example "make the paper into a square")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-result-TEMPL (xp (% W::PP (W::ptype W::into))))
     )
    (
     (LF-PARENT ONT::transformation) ;; GUM change new parent 20121027
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "make that triangle a big triangle")
     (TEMPL AGENT-affected-RESULT-TEMPL)
     (meta-data :origin fruitcarts :entry-date 20050427 :change-date nil :comments fruitcart-11-4)
     )
    (
     (LF-PARENT ONT::cause-effect) ;; GUM change new parent 20121027
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
       ;;;; at the moment this needs a different template from above; should try to consolidate
     (TEMPL agent-affected-effect-objcontrol-pred-templ)  
     (meta-data :origin medadvisor :entry-date 20011126)
     (example "aspirin makes him sick")
     )
    (
     (LF-PARENT ONT::cause-effect) ;; GUM change new parent 20121027
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
       ;;;; at the moment this needs a different template from above; should try to consolidate
     (TEMPL agent-EFFECT-affected-OBJCONTROL-TEMPL (xp (% W::VP (W::vform W::base))))
     (meta-data :origin medadvisor :entry-date 20011126)
     (example "aspirin makes him bleed")
     )
    (
     (LF-PARENT ONT::cause-effect) ;; GUM change new parent 20121027
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-effect-xp-templ (xp (% W::NP (w::gerund -))))
     )
    )
   )
))

