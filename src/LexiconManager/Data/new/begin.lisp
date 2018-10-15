;;;;
;;;; W::begin
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::begin
   (wordfeats (W::morph (:forms (-vb) :past W::began :pastpart W::begun))) ;:nom w::beginning)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (LF-PARENT ONT::startoff-begin-commence-start)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "He started to eat")
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (example "the managers began working")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::vp (W::vform W::ing))))
     )
#|    
     ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
      (lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he started them working")
     (TEMPL agent-effect-affected-OBJCONTROL-TEMPL (xp (% W::vp (W::vfrom W::ing))))
     )
|#
    ((LF-PARENT ONT::START)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "begin the meeting")
     (templ agent-neutral-xp-templ)
     )
    ((lf-parent ont::startoff-begin-commence-start) ;; 20120523 GUM change new parent
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "when can you begin")
     (templ agent-templ)
     )
     
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::beginning
   (wordfeats (W::VFORM W::ing) (W::MORPH (:forms NIL)))
   (SENSES
    (
     (LF-PARENT ONT::start-time)
     (example "Beginning Monday I work.")
     (TEMPL binary-constraint-S-templ (xp (% W::NP (W::case (? cas W::obj -)) (w::lf (% w::description (w::class ont::time-loc))))))
     )
    )
   )
))
