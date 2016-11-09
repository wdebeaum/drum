;;;;
;;;; w::give
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ((w::give (w::up))
    (wordfeats (W::morph (:forms (-vb) :past W::gave :pastpart w::given :ing W::giving)))
    (SENSES     
     ((LF-PARENT ONT::STOP)
      (EXAMPLE "I give up")
      (templ agent-templ)
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (meta-data :origin beetle2-onr2 :entry-date 20071204 :change-date nil :comments nil)
      )
     ((LF-PARENT ONT::STOP)
      (example "he gave up smoking")
      (TEMPL agent-effect-subjcontrol-templ (xp (% W::VP (W::vform W::ing))))
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (meta-data :origin beetle2-onr2 :entry-date 20071204 :change-date nil :comments nil)
      )
     ((LF-PARENT ONT::STOP)
      (EXAMPLE "He gave up the habit")
      (templ agent-effect-xp-templ)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     ((LF-PARENT ONT::SURRENDER)
      (EXAMPLE "He gave up a run to the other team.")
      (templ agent-affected-goal-optional-templ (xp (% W::pp (W::ptype W::to))))
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (meta-data :wn ("give_up%2:41:00"))
      )
     ))
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((w::give (w::off))
   (wordfeats (W::morph (:forms (-vb) :past W::gave)))
   (senses
    (
     (lf-parent ont::emit-giveoff-discharge) ;; 20120524 GUM change new parent
     (example "the spark plug gives off a spark")
     (TEMPL agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((w::give w::rise)
   (wordfeats (W::morph (:forms (-vb) :past W::gave)))
   (senses
    (
     (lf-parent ont::cause-produce-reproduce) 
     (example "The spark gave rise to a fire")
     (TEMPL agent-affected-create-templ (xp (% W::PP (W::ptype W::to))))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::give
   (wordfeats (W::morph (:forms (-vb) :past W::gave :pastpart W::given)))
   (SENSES

    ((lf-parent ont::giving)
     (example "give him a gift/a job")
;     (templ agent-affected-recipient-alternation-templ)
     (TEMPL AGENT-RECIPIENT-affected-TEMPL (xp (% W::NP (W::lf (% ?p (w::class (? !x ont::EVENT-OF-CHANGE)))))))
     ;; restructured this to allow non-movable things to be given
     (meta-data :origin calo :entry-date unknown :change-date 20040505 :comments calo-y1variants)
    )
   
    ((LF-PARENT ONT::giving)
     (example "give a gift to him")
;     (TEMPL agent-affected-goal-optional-templ) ; like grant,offer
     (TEMPL AGENT-AFFECTED-AR-TO-TEMPL)
     )
    
    #|
    (
     (LF-PARENT ONT::cause-effect) ;; GUM change new parent 20121027
     (EXAMPLE "aspirin gives me headaches")
     (TEMPL agent-affected-effect-templ);; GUM change new template 20121027
     (meta-data :origin medadvisor :entry-date 20011227 :change-date nil :comments nil)
     )

    (
     (LF-PARENT ONT::cause-effect)
     (EXAMPLE "he gave me a beating")
     (TEMPL agent-affected-effect-subjobjcontrol-templ)
     (meta-data :origin medadvisor :entry-date 20011227 :change-date nil :comments nil)
     )
    |#
    
    (
     (LF-PARENT ONT::cause-effect)
     (EXAMPLE "he gave me a beating")
     (TEMPL AGENT-AFFECTED-FORMAL-SUBJCONTROL-OBJ-TEMPL)
;     (TEMPL Agent-EFFECT-SUBJCONTROL-TEMPL (xp (% w::VP (w::vform w::ing))))
     (meta-data :origin medadvisor :entry-date 20011227 :change-date nil :comments nil)
     )
    )
   )
))

