;;;;
;;;; W::get
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :tags (:base500)
 :words (
  (W::get
   (wordfeats (W::morph (:forms (-vb) :ing W::getting :past W::got :pastpart W::gotten)))
   (SENSES
    ;;  basic sense of achieving a relationship to a position (from/to/around/...)
     ((LF-PARENT ONT::CAUSE-EFFECT)
     (example "the truck got to delta" "get out of the car")
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (TEMPL agent-effect-loc-subjcontrol-TEMPL)
      )
     ;; transitive version of the above
    ((LF-PARENT ONT::cause-effect)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin vn-analysis :entry-date unknown :change-date 20040617 :comments change-lf)
     (example "get the oranges into the truck" "I got the car from a dealer")
     (TEMPL agent-affected-effect-loc-objcontrol-templ)
     (preference 1)
     )

;    ;;;; Let's get the truck going.  Have the can opened.
    ((LF-PARENT ONT::MAKE-IT-SO)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-affected-objcontrol-templ (xp (% W::VP (W::vform (? x W::ing w::passive)))))
     )
    ;;;; Get Jane to clean the yard
    ((LF-PARENT ONT::MAKE-IT-SO)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-affected-objcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     )
    ;; I got to clean the yard
    ((LF-PARENT ONT::cause-effect)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-subjcontrol-templ)
     )

    ((LF-PARENT ONT::cause-effect) ;; GUM change new parent 20121027
     (example "he got them confused")
     (meta-data :origin medadvisor :entry-date 20020814 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-effect-objcontrol-pred-templ)
     )
  
    ((lf-parent ont::acquire)
     (example "get the book used")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL agent-affected-objcontrol-pred-templ)
     )

    ((LF-PARENT ONT::BECOME)
     (meta-data :origin medadvisor :entry-date 20020814 :change-date nil :comments nil)
     (example "he gets nauseous")
     (TEMPL AFFECTED-PRED-TEMPL)
     )
    ((LF-PARENT ONT::come-to-UNDERSTAND)
     (example "I get it")
     (TEMPL agent-theme-xp-templ)
     (PREFERENCE 0.97)
     )
    
    ((LF-PARENT ONT::acquire)
     (example "he got the tapes")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (preference .98) ;; prefer advbl attachment to verb with cause-effect sense
     )
    #||((LF-PARENT ONT::acquire) ;; beneficiary
     (example "get me a computer")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo :entry-date 20040401 :change-date nil :comments y1-dialog-variations :vn ("get-13.5.1-1"))
     (TEMPL AGENT-recipient-affected-TEMPL)
     )||#
    ((LF-PARENT ONT::avoiding)
     (example "get around the problem")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo :entry-date 20041201 :change-date nil :comments caloy2)
     (TEMPL AGENT-neutral-XP-TEMPL (xp (% W::PP (W::ptype W::around))))
     )
    ((LF-PARENT ONT::PASSIVE)
     (example "abrams got hired by brown")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (TEMPL PASSIVE-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   ((W::get (W::up))
   (wordfeats (W::morph (:forms (-vb) :ing W::getting :past W::got :pastpart W::gotten)))
   (SENSES
    ((EXAMPLE "I don't like getting up in the middle of the night")
     (meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
     (LF-PARENT ONT::BODY-MOVEMENT-self)
     (TEMPL agent-TEMPL)
     )
    ((EXAMPLE "he got him up")
     (meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
     (LF-PARENT ONT::BODY-MOVEMENT-self)
     (TEMPL agent-affected-xp-templ)
     )
    )
    )
   ))



#||(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::get (W::off))
   (wordfeats (W::morph (:forms (-vb) :ing W::getting :past W::got :pastpart W::gotten)))
   (SENSES
    ;;;; get the people off
    ;;;; prefer 'obtain' sense without a directional
    ((EXAMPLE "we can get more people off quickly")
     (LF-PARENT ONT::cause-off)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic) (F::trajectory +))
     )
    )
   )
))||#

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ((w::get w::together)
    (wordfeats (W::morph (:forms (-vb) :past w::got :nom (w::get w::together))))
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3 :vn ("herd-47.5.2") :wn ("congregate%2:38:00"))
     (LF-PARENT ONT::meet)
     (example "they got together at the church")
     (TEMPL agent-plural-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::get W::rid)
   (wordfeats (W::morph (:forms (-vb) :past W::got :pastpart W::gotten)))
   (SENSES
    ;;;; get rid of s.t.
    ((LF-PARENT ONT::discard)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL (xp (% W::PP (W::ptype W::of))))
     )
    )
   )
))

;; This should be compositional

#||(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ((W::get w::dressed)
       (wordfeats (W::morph (:forms (-vb) :past w::got :ing w::getting)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (EXAMPLE "he got dressed quickly")
     (LF-PARENT ONT::PUT-ON)
     (templ agent-templ)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (EXAMPLE "he got dressed the child")
     (LF-PARENT ONT::PUT-ON)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))||#
