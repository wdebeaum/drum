;;;;
;;;; w::go
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::go
  (senses
   ((LF-PARENT ONT::board-game)
    (TEMPL mass-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :tags (:base500)
 :words (
  (W::go
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((lf-parent ont::undergo-action)
     (example "I kept going")
     (TEMPL affected-TEMPL)
     (preference .97)  ;; this really should be a last resort
     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-followup))
    ((lf-parent ont::occurring)
     (example "The interview went well")
     (TEMPL neutral-TEMPL) (SYNTAX (ADVBL-NECESSARY +))
     (preference 0.98)
     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-followup))
    ((lf-parent ont::execute)
     (example "he went jogging")
     (templ agent-effect-subjcontrol-templ (xp (% W::VP (W::vform W::ing))))
     (meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil))
     
    ((lf-parent ont::execute)
     (example "he went for a walk")
     (templ agent-effect-xp-templ (xp (% W::pp (W::ptype (? pt w::for)))))
     (meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil))     
    
    ((LF-PARENT ONT::MOVE)
     (SEM (F::aspect F::unbounded) (F::time-span F::extended))
     (example "go to amazon dot com" "go by the store")
     (meta-data :origin plow :entry-date 20050315 :change-date nil :comments nil)
     (TEMPL AGENT-TEMPL)
     )
    ((meta-data :origin plow :entry-date 20060113 :change-date nil :comments pqs)
     (EXAMPLE "this goes here" "the title goes in the textbox")
     (LF-PARENT ONT::should-be-at)
     (TEMPL neutral-location-TEMPL)
     )
    ((LF-PARENT ONT::startoff-begin-commence-start)
     (example "go to/and fix the power lines")
     (SEM (F::aspect F::unbounded) (F::time-span F::extended))
     (TEMPL AGENT-effect-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype (? ct W::s-and)))))
     (PREFERENCE 0.97)    ;;;; use any other possible sense first
     )
    ;;;; for imperatives only
    ((EXAMPLE "Go find it")
     (LF-PARENT ONT::startoff-begin-commence-start)
     (SEM (F::aspect F::unbounded) (F::time-span F::extended))
     (TEMPL AGENT-effect-SUBJCONTROL-TEMPL (xp (% W::vp (W::vform W::base) (w::complex -))))  
     (SYNTAX (W::morph (:forms NIL)) (W::vform W::base))
     (PREFERENCE 0.97)    ;;;; use any other possible sense first
     )
    #||((LF-PARENT ONT::DEPART)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "This ugly carpet goes.")
     (TEMPL AGENT-TEMPL)
     )||#
   
   ))
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
#|
  ((W::go w::through)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((lf-parent ont::undergo-action)
     (example "The fax went through / the paper went through changes")
     (TEMPL affected-neutral-OPTIONAL-TEMPL)
     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-followup))
    ))
|#
  ((W::go w::for)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((example "let's go for/with the mac")
     (LF-PARENT ONT::SELECT)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ (xp (% W::pp (W::ptype (? pt w::for W::with)))))
     (meta-data :origin calo :entry-date 20040412 :change-date nil :comments calo-y1v4)
     )
    ))
  ((W::go w::with)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((example "let's go for/with the mac")
     (LF-PARENT ONT::SELECT)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ (xp (% W::pp (W::ptype (? pt w::for W::with)))))
     (meta-data :origin calo :entry-date 20040412 :change-date nil :comments calo-y1v4)
     )
    ))
  ))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::go W::on)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ;;;; Go on to Delta / The truck went on to Delta
    ((LF-PARENT ONT::GO-ON)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AFFECTED-TEMPL)
              ;;; Myrosia 20040805 lowered the preference so that this sense doesn't interfere unless main selectional restrictions
     ;;; so move sense should only show up in boosted versions of the domain
     (preference 0.95)
     )
    ((EXAMPLE "the rioting is going on")
     (LF-PARENT ONT::occurring)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL neutral-TEMPL)
     )
    ;;;; go on with the story
    ((LF-PARENT ONT::ACTIVITY-ONGOING)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-neutral-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::with))))
     (meta-data :origin bee :entry-date ? :change-date 20040805 :comments portability-followup)
     )
    ((LF-PARENT ONT::START-OBJECT)
     (TEMPL affected-TEMPL)
     (meta-data :origin beetle :entry-date 20010516 :change-date nil :comments pilot4)
     (example "the bulb went on")
     )
    )
   )
))

#||(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::go w::back)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((LF-PARENT ONT::GO-BACK)
     (example "go back to avon")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-source-optional-templ (xp (% w::pp (w::ptype w::to))))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
 ((W::go w::up)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((lf-parent ont::increase)
     (example "his weight went up")
     (TEMPL AFFECTED-TEMPL)
     (meta-data :origin cardiac :entry-date 20090226 :change-date 20090504 :comments nil))
    ))
))||#

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
 ((W::go w::down)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((lf-parent ont::decrease)
     (example "his weight went down")
     (TEMPL AFFECTED-TEMPL)
     (meta-data :origin cardiac :entry-date 20090226 :change-date 20090504 :comments nil))
    ))
))

#||(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::go W::out)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((LF-PARENT ONT::social-activity)
     (example "I want to go out tonight")
     (meta-data :origin medadvisor :entry-date 2003???? :change-date nil :comments nil)
     (TEMPL AGENT-TEMPL)
     )
    ((LF-PARENT ONT::extinguish)
     (TEMPL AFFECTED-TEMPL)
     (meta-data :origin beetle :entry-date 20050216 :change-date nil :comments mockup-1)
     (example "the lamp went out")
     ))
   )
))||#

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::go W::off)
   (wordfeats (W::morph (:forms (-vb) :3s W::goes :past W::went :pastpart W::gone)))
   (SENSES
    ((LF-PARENT ONT::START-OBJECT)
     (TEMPL affected-TEMPL)
     (meta-data :origin beetle :entry-date 20080516 :change-date nil :comments pilot2)
     (example "the bulb went off")
     ))
   )
))

