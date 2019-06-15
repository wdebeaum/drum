;;;;
;;;; W::begin
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::begin
   (wordfeats (W::morph (:forms (-vb) :past W::began :pastpart W::begun))) ;:nom w::beginning)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (LF-PARENT ONT::start)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "He started to eat")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (lf-parent ont::start)
     (example "the managers began working")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% W::vp (W::vform W::ing))))
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
    ((lf-parent ont::start) ;; 20120523 GUM change new parent
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "when can you begin")
     (templ agent-templ)
     )
     
    )
   )
))
