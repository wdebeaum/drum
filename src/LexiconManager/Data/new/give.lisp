;;;;
;;;; w::give
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ((w::give (w::up))
    (wordfeats (W::morph (:forms (-vb) :past W::gave :pastpart w::given :ing W::giving)))
    (SENSES     
     ((EXAMPLE "I give up")
      (LF-PARENT ONT::STOP)
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (templ agent-templ)
      (meta-data :origin beetle2-onr2 :entry-date 20071204 :change-date nil :comments nil)
      )
     ((LF-PARENT ONT::STOP)
      (example "he gave up smoking")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL agent-effect-subjcontrol-templ (xp (% W::VP (W::vform W::ing))))
      (meta-data :origin beetle2-onr2 :entry-date 20071204 :change-date nil :comments nil)
      )
     ((EXAMPLE "He gave up the habit")
      (LF-PARENT ONT::STOP)
      (templ agent-effect-xp-templ)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
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
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the spark plug gives off a spark")
     (TEMPL agent-affected-xp-templ)
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
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-create-templ (xp (% W::PP (W::ptype W::to))))
     (example "The spark gave rise to a fire")
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
;     (templ agent-affected-recipient-alternation-templ)
     (TEMPL AGENT-RECIPIENT-affected-TEMPL (xp (% W::NP (W::lf (% ?p (w::class (? !x ont::EVENT-OF-CHANGE)))))))
     (example "give him a gift/a job")
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
     (TEMPL agent-affected-effect-templ);; GUM change new template 20121027
     (EXAMPLE "aspirin gives me headaches")
     (meta-data :origin medadvisor :entry-date 20011227 :change-date nil :comments nil)
     )

    (
     (LF-PARENT ONT::cause-effect)
     (TEMPL agent-affected-effect-subjobjcontrol-templ)
     (EXAMPLE "he gave me a beating")
     (meta-data :origin medadvisor :entry-date 20011227 :change-date nil :comments nil)
     )
    |#
    
    (
     (LF-PARENT ONT::cause-effect)
     (TEMPL AGENT-AFFECTED-FORMAL-SUBJCONTROL-OBJ-TEMPL)
;     (TEMPL Agent-EFFECT-SUBJCONTROL-TEMPL (xp (% w::VP (w::vform w::ing))))
     (EXAMPLE "he gave me a beating")
     (meta-data :origin medadvisor :entry-date 20011227 :change-date nil :comments nil)
     )

    
    )
   )
))

