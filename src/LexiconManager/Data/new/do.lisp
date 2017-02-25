;;;;
;;;; W::do
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::do
   (wordfeats (W::morph (:forms (-vb) :past W::did :pastpart W::done :3s W::does)))
   (SENSES
    ;;;; the rock did fall on a truck
    ((LF-PARENT ONT::DO)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::do)
     (TEMPL DO-TEMPL)
     )
    ;;;; I do, I don't
    ((LF-PARENT ONT::DO)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::do)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::changesem -))
     )
    ;;;; I did it. I did the a.
    ;;;; what should the temporal features of 'do' be? for now, treat like activity
    ((LF-PARENT ONT::EXECUTE)
     (example "I did it." "I did the task")
     (SEM (F::Aspect F::Unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (example "I did the activity")
     )
      )
    )
))

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
	  (w::do
	     (wordfeats (W::morph (:forms (-vb) :past W::did :pastpart W::done :3s W::does)))
	      (senses
	       ((lf-parent ont::objective-influence)
		(example "It did damage to a battery")
		(TEMPL agent-RESULT-TO-AFFECTED-optional-TEMPL)
		(preference .98)
		(meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-followup)
		)
	       ))
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::do W::away)
   (wordfeats (W::morph (:forms (-vb) :past W::did :pastpart W::done :3s W::does)))
   (SENSES
    ((LF-PARENT ONT::discard) ;; like get rid of, eliminate
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     (example "Let's do away with the course on Akkadian hieroglyphics")
     (meta-data :origin jr :entry-date 20120806 :change-date nil :comments gloss-owl)
     )
    )
   )
))
