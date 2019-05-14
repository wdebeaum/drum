;;;;
;;;; W::keep
;;;;

(define-words :pos W::v :templ AGENT-affected-nogerund-TEMPL
 :tags (:base500)
 :words (
(W::keep
   (wordfeats (W::morph (:forms (-vb) :ing W::keeping :past W::kept)))
   (SENSES
    ;;;; we may need to devise special case for keeping at a particular location
    ((LF-PARENT ONT::maintain-keep)
     (example "keep the room clean")
     (meta-data :origin trips :entry-date unknown :change-date 20090220 :comments beetle-pilots)
     (SEM (F::Locative F::Located) (F::Time-span F::extended))
     (templ agent-affected-theme-objcontrol-pred-templ)
     )
    ((example "are you keeping busy")
     (LF-PARENT ONT::activity-ongoing)
     (TEMPL agent-theme-complex-subjcontrol-templ)
     (meta-data :origin chf :entry-date 20070817 :change-date 20090220 :comments chf-dialogues)
     )
    ((lf-parent ont::maintain-keep
		)
     (SEM (F::Cause F::Agentive) (F::Locative F::Located) (F::Time-span F::extended))
     (example "Keep walking")
     (TEMPL Agent-EFFECT-SUBJCONTROL-TEMPL (xp (% w::VP (w::vform w::ing))))
     (meta-data :origin bee :entry-date 20040805 :change-date 20090220 :comments portability-followup)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("keep-15.2") :wn ("keep%2:40:10" "keep%2:40:12" "keep%2:40:13"))
     (LF-PARENT ONT::retain)
     (example "keep the oranges")
     (preference .98)  
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (templ agent-affected-xp-templ)
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It keeps him from doing something")
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::keep w::in w::touch)
   (SENSES
    ((LF (W::GOODBYE))
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CLOSE))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::keep (W::down))
   (wordfeats (W::morph (:forms (-vb) :past W::kept)))
   (SENSES
    ((LF-PARENT ONT::keep-down-vomit)
     (example "I’d eat something if I thought I could keep it down.")
     )
    )
   )
))
