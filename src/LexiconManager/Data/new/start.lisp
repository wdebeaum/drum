;;;;
;;;; W::START
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::START
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ont::pos-start-of-trajectory); ONT::LINE-DEPENDENT-LOCATION)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::START
   (wordfeats (W::morph (:forms (-vb) :nom w::start)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     ;;(LF-PARENT ONT::START)
     (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he started to eat")
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     ;;(LF-PARENT ONT::START)
     (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (example "the managers started working")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::vp (W::vform W::ing))))
     )
    ((LF-PARENT ont::startoff-begin-commence-start)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL (xp (% w::vp (w::vform w::ing))))
     (example "start the visitors working")
     )
    (
     (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "start the action")
     (templ agent-effect-xp-templ)
     )
    (;;(LF-PARENT ONT::START)
     (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "when can you start")
     (templ agent-templ)
     )
    (;;(LF-PARENT ONT::START)
     (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he started the truck")
     (templ agent-affected-xp-templ)
     )
    ;; these next ones should be handled compositionally
    #||((lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (meta-data :origin fruit-carts :entry-date 20050429 :change-date nil :comments fruitcarts-11-2)
     (example "start with the triangle")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-xp-templ (xp (% W::pp (W::ptype W::with))))
     )
    ((lf-parent ont::start-at-loc) ;; 20120523 GUM change new parent
     (example "the path starts in the forest")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL neutral-location-templ)
     )||#
    )
   )
))

(define-words :pos W::v 
 :words (
 ((W::start (W::off))
   (SENSES
    ((LF-PARENT ONT::START)
     (meta-data :origin fruit-carts :entry-date 20050331 :change-date nil :comments fruitcarts-11-3)
     (example "start off with the triangle")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-xp-templ (xp (% W::pp (W::ptype W::with))))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::start (W::over))
   (SENSES
    ((LF-PARENT ONT::RESTART)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ)
     (example "start over")
     )
    ((LF-PARENT ONT::RESTART)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the program started over")
     (TEMPL affected-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::start (W::again))
   (SENSES
    ((LF-PARENT ONT::RESTART)
     (example "let's start again")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ)
     )
    ((LF-PARENT ONT::RESTART)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the rain has started again")
     (TEMPL affected-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  ((W::start (w::off))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("begin-55.1-1"))
    ;; (LF-PARENT ONT::start)
 ; like begin
     (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (example "he started off the discussion")
     (templ agent-affected-xp-templ)
     )

    ((lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (example "the managers started off working")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-subjcontrol-templ (xp (% w::vp (w::vform w::ing))))
     )
   ))
))

