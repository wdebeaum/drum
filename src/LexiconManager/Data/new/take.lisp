;;;;
;;;; :::take
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::take (W::off))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((LF-PARENT ONT::cause-off)
     (example "take off the coat/take the coat off")
     (SEM (F::Aspect F::bounded) (F::time-span F::extended) (F::cause F::agentive))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::take (W::out))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((EXAMPLE "take out those lights")
     (LF-PARENT ONT::cause-out-of)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic) (F::cause F::agentive))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::take (W::in))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((EXAMPLE "take in the garbage")
     (LF-PARENT ONT::take-in)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic) (F::cause F::agentive))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::take
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((EXAMPLE "He took the aspirin")
     (LF-PARENT ONT::consume)
     (SEM (F::cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    
    ;;;; Take a path, e.g., The truck takes the route to Avon
    ((LF-PARENT ONT::MOVE-by-means)
     (SEM (F::Aspect F::bounded) (F::time-span F::extended))
     (example "The truck takes the route to Avon")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    
    ((LF-PARENT ONT::TAKE-TIME)
     (example "the plan took [him] 5 hours")
     (TEMPL NEUTRAL-EXTENT-TEMPL)
     )

    ((LF-PARENT ONT::TAKE-TIME)
     (example "it took (me) 5 hours to  complete the plan")
     (TEMPL EXPLETIVE-NEUTRAL-EXTENT-FORMAL-2-OPTIONAL-4-OBJCONTROL-TEMPL)
     (SEM (F::Aspect F::stage-level))
     )
    
    ((LF-PARENT ONT::TAKE-TIME)
     (example "he took 5 hours to work") 
     (TEMPL NEUTRAL-EXTENT-FORMAL-SUBJCONTROL-TEMPL)
     )
   
    ((LF-PARENT ONT::create)
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::time-span F::extended))
     (example "take notes; take pictures")
     (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
     )

    #|
    ;;;; take a city the preference is low, so that motion senses,
    ;;;; which are more salient for mobile objects, come up on top
    ((LF-PARENT ONT::appropriate)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
     (PREFERENCE 0.96)
     )
    |#
    
    ((LF-PARENT ONT::ACQUIRE)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(preference 1.0) ;; the default 
     (example "take the box")
     (template AGENT-AFFECTED-XP-TEMP)
     )

    ((LF-PARENT ONT::cause-move)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "take the bird out of the cage")
     (template AGENT-AFFECTED-XP-TEMP)
     )
    
    ((LF-PARENT ONT::is-compatible-with)
     (SEM (F::Time-span F::extended) (f::trajectory -))
     (example "that projector takes european voltage")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (meta-data :origin calo :entry-date 20050308 :change-date nil :comments projector-corpus)
     )
    ((LF-PARENT ONT::take-execute)
     (example "take action" "take a shower")
     (meta-data :origin asma :entry-date 20111005)
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::take W::care)
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken)))
   (SENSES
    (;;(LF-PARENT ONT::managing)
      ;(lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (lf-parent ont::taking-care-of)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he took care of the problem")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL (xp (% W::PP (W::ptype W::of))))
     )    
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::take (w::away))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    (
     (meta-data :origin particle-verbs :entry-date 20100201)
     (LF-PARENT ONT::cause-come-from)
     (example "take away the cargo" "take it away from the truck")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 ((W::take (w::over))
  (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking :nom (w::take w::over))))
   (SENSES
    (
     (meta-data :origin particle-verbs :entry-date 20100201)
     (LF-PARENT ONT::appropriate)
     (example "take the project over" "take over the project")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   ((W::take (w::up))
    (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((meta-data :origin particle-verbs :entry-date 20100201)
     (LF-PARENT ONT::appropriate)
     (example "take the slack up" "take up the slack")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL )
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   ((W::take (w::on))
     (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((meta-data :origin particle-verbs :entry-date 20100201)
     ;;(LF-PARENT ONT::accept)
     ;(lf-parent ont::take-on) ;; 20120524 GUM change new type
     (LF-PARENT ONT::APPROPRIATE)
     (example "take the project on" "take on the project")
     (SEM (F::Aspect F::bounded) )
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::take w::a w::chance)
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::rely)
     (TEMPL agent-neutral-xp-templ (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     )
    )
   )
))

